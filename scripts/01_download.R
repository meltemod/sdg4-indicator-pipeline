# Step 1: download UIS bulk data and extract the indicators the views need.
#
#   Rscript scripts/01_download.R
#
# Source: UIS Bulk Data Download Service (BDDS), one zip per dataset, pinned
# to the release in config.yml (bdds.release). Zips are cached in
# data/raw/<release>/ and never re-downloaded, so a re-run is offline and
# reproduces the same input exactly.
#
# Output: data/interim/indicators_long.csv, one row per
#   indicator x geography x year, with columns
#   indicator_id, indicator_label, dataset, geo_type, geo_code, geo_name,
#   year, value, magnitude, qualifier
#
# What happens, in order:
#   1. Work out which indicators are needed, and whether each view wants
#      country-level or region-level data.
#   2. Download each dataset's zip (once) and unzip it.
#   3. Look up which dataset (SDG or OPRI) each indicator lives in.
#   4. Read only the data files that hold something we need, keep only the
#      needed indicators, and reshape them into one common format.
#   5. Stack everything into one long table and save it.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Paths in config.yml are relative to the project root, so stop early with a
# clear message if the script is run from somewhere else.
if (!file.exists("config.yml")) stop("Run from the project root.", call. = FALSE)
cfg <- yaml::read_yaml("config.yml")

# Each release gets its own folder (e.g. data/raw/202611/), so moving to a
# newer UIS release never overwrites the files an earlier run used.
release  <- cfg$bdds$release
raw_dir  <- file.path(cfg$paths$raw, release)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$paths$interim, recursive = TRUE, showWarnings = FALSE)

# ---- 1. what the views need -----------------------------------------------
# Build a small lookup table with one row per (view, indicator), e.g.
#
#   view            indicator_id  geo_type
#   out_of_school   ROFST.1.CP    country
#   adult_literacy  LR.AG15T99    region
#
# geo_type decides which UIS file we read later: "country" -> the NATIONAL
# data file, "region" -> the REGIONAL data file.
#
# In config.yml, `geo` is either the text "countries" or a list such as
# {regions: [...]}, which is why the check is identical(geo, "countries").
views <- cfg$views
needed <- bind_rows(lapply(names(views), function(v) {
  geo <- views[[v]]$geo
  tibble(
    view         = v,
    indicator_id = unlist(views[[v]]$indicators),
    geo_type     = if (identical(geo, "countries")) "country" else "region"
  )
}))

# Every region named by any view, e.g. "SDG: World". Views that use
# `geo: countries` have no region list (geo is plain text, not a list), so
# they contribute nothing here.
regions_needed <- unique(unlist(lapply(views, function(v) {
  if (is.list(v$geo)) v$geo$regions else NULL
})))

# ---- 2. download + unzip (cached) -----------------------------------------
for (ds in cfg$bdds$datasets) {
  zip_path <- file.path(raw_dir, paste0(ds, ".zip"))

  # Download only if we don't already have this release's zip.
  if (!file.exists(zip_path)) {
    url <- sprintf("%s/%s/%s.zip", cfg$bdds$base_url, release, ds)
    message("Downloading ", url)
    # The zips are 40-60 MB. R's default 60-second timeout can cut a slow
    # download short, so raise it for this call and restore it afterwards.
    old <- options(timeout = max(600, getOption("timeout")))
    download.file(url, zip_path, mode = "wb", quiet = TRUE)   # "wb": binary-safe on Windows
    options(old)
  } else {
    message("Using cached ", zip_path)
  }

  # Unzip everything except *_METADATA.csv. Those files are 250-340 MB of
  # footnotes that the pipeline doesn't use, so extracting them would only
  # waste time and disk space.
  out_dir <- file.path(raw_dir, ds)
  members <- utils::unzip(zip_path, list = TRUE)$Name
  members <- members[!grepl("_METADATA\\.csv$", members)]
  # Skip unzipping if every file is already there from an earlier run.
  if (!all(file.exists(file.path(out_dir, members)))) {
    utils::unzip(zip_path, files = members, exdir = out_dir, overwrite = TRUE)
  }
}

# Path to one CSV inside an unzipped dataset, e.g.
#   ds_file("SDG", "LABEL") -> data/raw/202611/SDG/SDG_LABEL.csv
ds_file <- function(ds, suffix) file.path(raw_dir, ds, sprintf("%s_%s.csv", ds, suffix))

# Read a UIS CSV and make its column names consistent.
# Two quirks in the UIS files make this necessary:
#   - the REGIONAL files use lower-case headers (indicator_id), the NATIONAL
#     files upper-case (INDICATOR_ID), so we upper-case everything;
#   - some files start with an invisible "byte order mark" character
#     (﻿), which would otherwise stick to the first column name.
read_upper <- function(path, ...) {
  x <- read_csv(path, show_col_types = FALSE, progress = FALSE, ...)
  names(x) <- toupper(sub("^﻿", "", names(x)))
  x
}

# ---- 3. map each indicator to the dataset that carries it -----------------
# Each dataset ships a LABEL file listing its indicator codes and their
# English names. Stacking the label files gives a lookup of
# indicator_id -> (label, dataset), so config.yml only has to name the
# indicator, not where it lives. For example, ILLPOP.AG15T99 is found in OPRI.
labels <- bind_rows(lapply(cfg$bdds$datasets, function(ds) {
  read_upper(ds_file(ds, "LABEL"), col_types = "cc") |>
    transmute(indicator_id = INDICATOR_ID,
              indicator_label = trimws(INDICATOR_LABEL_EN),   # UIS labels have a leading space
              dataset = ds)
}))

# Attach the dataset to each needed indicator. A code that matches no label
# file is almost always a typo in config.yml, so stop and name it rather
# than producing a workbook with an empty column.
needed <- left_join(needed, labels, by = "indicator_id")
unknown <- needed$indicator_id[is.na(needed$dataset)]
if (length(unknown) > 0L) {
  stop("Indicator(s) not found in any BDDS label file: ",
       paste(unique(unknown), collapse = ", "), call. = FALSE)
}

# ---- 4. read only the data files that are needed --------------------------
# Read every column as text first. Column types can only be set after the
# names are upper-cased (see read_upper), because the same column is called
# YEAR in one file and year in another.
data_cols <- cols(.default = col_character())

# Country code -> English name (e.g. AFG -> Afghanistan). The COUNTRY file is
# the same in every dataset, so reading the first one is enough.
country_names <- read_upper(ds_file(cfg$bdds$datasets[[1]], "COUNTRY"), col_types = "cc") |>
  select(COUNTRY_ID, COUNTRY_NAME_EN)

# Read one data file (dataset x geo_type), keep only the indicators we need,
# and return it in the common column layout:
#   indicator_id, geo_code, geo_name, year, value, magnitude, qualifier
# Returns NULL when nothing is needed from this file, so the larger files are
# never opened needlessly. OPRI's national file alone is ~190 MB.
pull_data <- function(ds, geo_type) {
  ids <- unique(needed$indicator_id[needed$dataset == ds & needed$geo_type == geo_type])
  if (length(ids) == 0L) return(NULL)

  suffix <- if (geo_type == "country") "DATA_NATIONAL" else "DATA_REGIONAL"
  message(sprintf("Reading %s_%s.csv for %d indicator(s)", ds, suffix, length(ids)))

  x <- read_upper(ds_file(ds, suffix), col_types = data_cols) |>
    filter(INDICATOR_ID %in% ids) |>
    mutate(YEAR = as.integer(YEAR), VALUE = as.numeric(VALUE))

  if (geo_type == "country") {
    # National rows carry only a country code, so join the country name on.
    x |>
      left_join(country_names, by = "COUNTRY_ID") |>
      transmute(indicator_id = INDICATOR_ID, geo_code = COUNTRY_ID,
                geo_name = COUNTRY_NAME_EN, year = YEAR, value = VALUE,
                magnitude = MAGNITUDE, qualifier = QUALIFIER)
  } else {
    # Regional rows carry the full region name, e.g. "SDG: Sub-Saharan Africa".
    # Keep only the regions a view asked for. The full name becomes the code,
    # and the part after "<org>: " becomes the display name ("Sub-Saharan Africa").
    x |>
      filter(REGION_ID %in% regions_needed) |>
      transmute(indicator_id = INDICATOR_ID, geo_code = REGION_ID,
                geo_name = sub("^[^:]+:\\s*", "", REGION_ID), year = YEAR,
                value = VALUE, magnitude = MAGNITUDE, qualifier = QUALIFIER)
  }
}

# ---- 5. stack, tidy, save -------------------------------------------------
# One pull_data() call per distinct (dataset, geo_type) pair, e.g.
# (SDG, country), (SDG, region), (OPRI, region). Each result is tagged with
# its geo_type, and the pieces are stacked into one long table.
combos <- distinct(needed, dataset, geo_type)
long <- bind_rows(lapply(seq_len(nrow(combos)), function(i) {
  d <- pull_data(combos$dataset[i], combos$geo_type[i])
  if (!is.null(d)) d$geo_type <- combos$geo_type[i]
  d
})) |>
  # Add the indicator's English label and source dataset.
  left_join(distinct(labels, indicator_id, indicator_label, dataset), by = "indicator_id") |>
  # UIS leaves magnitude/qualifier empty for most rows. Use "" rather than NA
  # so later steps can compare them as text without NA handling.
  mutate(across(c(magnitude, qualifier), ~ coalesce(.x, ""))) |>
  select(indicator_id, indicator_label, dataset, geo_type, geo_code, geo_name,
         year, value, magnitude, qualifier) |>
  arrange(indicator_id, geo_type, geo_code, year)

# A region name in config.yml must match UIS's spelling exactly, including
# the "SDG: " prefix. If any region found no rows, stop and say which one,
# instead of silently producing a chart with a missing line.
missing_regions <- setdiff(regions_needed, long$geo_code[long$geo_type == "region"])
if (length(missing_regions) > 0L) {
  stop("Region(s) not found in regional data: ",
       paste(missing_regions, collapse = ", "),
       "\n  Check spelling against ", ds_file("SDG", "REGION"), call. = FALSE)
}

out <- file.path(cfg$paths$interim, "indicators_long.csv")
write_csv(long, out, na = "")

# Print row counts per indicator so a run shows at a glance what was found.
summary_tbl <- count(long, indicator_id, geo_type, name = "rows")
print(as.data.frame(summary_tbl), row.names = FALSE)
message("Wrote ", nrow(long), " rows to ", out)
