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

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

if (!file.exists("config.yml")) stop("Run from the project root.", call. = FALSE)
cfg <- yaml::read_yaml("config.yml")

release  <- cfg$bdds$release
raw_dir  <- file.path(cfg$paths$raw, release)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$paths$interim, recursive = TRUE, showWarnings = FALSE)

# ---- what the views need --------------------------------------------------
views <- cfg$views
needed <- bind_rows(lapply(names(views), function(v) {
  geo <- views[[v]]$geo
  tibble(
    view         = v,
    indicator_id = unlist(views[[v]]$indicators),
    geo_type     = if (identical(geo, "countries")) "country" else "region"
  )
}))
regions_needed <- unique(unlist(lapply(views, function(v) {
  if (is.list(v$geo)) v$geo$regions else NULL
})))

# ---- download + unzip (cached) ------------------------------------------
# The *_METADATA.csv files (~250-340 MB each) hold footnotes only; skipped.
for (ds in cfg$bdds$datasets) {
  zip_path <- file.path(raw_dir, paste0(ds, ".zip"))
  if (!file.exists(zip_path)) {
    url <- sprintf("%s/%s/%s.zip", cfg$bdds$base_url, release, ds)
    message("Downloading ", url)
    old <- options(timeout = max(600, getOption("timeout")))
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
    options(old)
  } else {
    message("Using cached ", zip_path)
  }
  out_dir <- file.path(raw_dir, ds)
  members <- utils::unzip(zip_path, list = TRUE)$Name
  members <- members[!grepl("_METADATA\\.csv$", members)]
  if (!all(file.exists(file.path(out_dir, members)))) {
    utils::unzip(zip_path, files = members, exdir = out_dir, overwrite = TRUE)
  }
}

ds_file <- function(ds, suffix) file.path(raw_dir, ds, sprintf("%s_%s.csv", ds, suffix))

# Regional files use lower-case headers, national files upper-case. Normalise.
read_upper <- function(path, ...) {
  x <- read_csv(path, show_col_types = FALSE, progress = FALSE, ...)
  names(x) <- toupper(sub("^﻿", "", names(x)))
  x
}

# ---- map each indicator to the dataset that carries it ------------------
labels <- bind_rows(lapply(cfg$bdds$datasets, function(ds) {
  read_upper(ds_file(ds, "LABEL"), col_types = "cc") |>
    transmute(indicator_id = INDICATOR_ID,
              indicator_label = trimws(INDICATOR_LABEL_EN),
              dataset = ds)
}))

needed <- left_join(needed, labels, by = "indicator_id")
unknown <- needed$indicator_id[is.na(needed$dataset)]
if (length(unknown) > 0L) {
  stop("Indicator(s) not found in any BDDS label file: ",
       paste(unique(unknown), collapse = ", "), call. = FALSE)
}

# ---- read only the data files that are needed ---------------------------
# Read as text, then type after normalising names (header case differs by file).
data_cols <- cols(.default = col_character())

country_names <- read_upper(ds_file(cfg$bdds$datasets[[1]], "COUNTRY"), col_types = "cc") |>
  select(COUNTRY_ID, COUNTRY_NAME_EN)

pull_data <- function(ds, geo_type) {
  ids <- unique(needed$indicator_id[needed$dataset == ds & needed$geo_type == geo_type])
  if (length(ids) == 0L) return(NULL)
  suffix <- if (geo_type == "country") "DATA_NATIONAL" else "DATA_REGIONAL"
  message(sprintf("Reading %s_%s.csv for %d indicator(s)", ds, suffix, length(ids)))
  x <- read_upper(ds_file(ds, suffix), col_types = data_cols) |>
    filter(INDICATOR_ID %in% ids) |>
    mutate(YEAR = as.integer(YEAR), VALUE = as.numeric(VALUE))
  if (geo_type == "country") {
    x |>
      left_join(country_names, by = "COUNTRY_ID") |>
      transmute(indicator_id = INDICATOR_ID, geo_code = COUNTRY_ID,
                geo_name = COUNTRY_NAME_EN, year = YEAR, value = VALUE,
                magnitude = MAGNITUDE, qualifier = QUALIFIER)
  } else {
    x |>
      filter(REGION_ID %in% regions_needed) |>
      transmute(indicator_id = INDICATOR_ID, geo_code = REGION_ID,
                geo_name = sub("^[^:]+:\\s*", "", REGION_ID), year = YEAR,
                value = VALUE, magnitude = MAGNITUDE, qualifier = QUALIFIER)
  }
}

combos <- distinct(needed, dataset, geo_type)
long <- bind_rows(lapply(seq_len(nrow(combos)), function(i) {
  d <- pull_data(combos$dataset[i], combos$geo_type[i])
  if (!is.null(d)) d$geo_type <- combos$geo_type[i]
  d
})) |>
  left_join(distinct(labels, indicator_id, indicator_label, dataset), by = "indicator_id") |>
  mutate(across(c(magnitude, qualifier), ~ coalesce(.x, ""))) |>
  select(indicator_id, indicator_label, dataset, geo_type, geo_code, geo_name,
         year, value, magnitude, qualifier) |>
  arrange(indicator_id, geo_type, geo_code, year)

missing_regions <- setdiff(regions_needed, long$geo_code[long$geo_type == "region"])
if (length(missing_regions) > 0L) {
  stop("Region(s) not found in regional data: ",
       paste(missing_regions, collapse = ", "),
       "\n  Check spelling against ", ds_file("SDG", "REGION"), call. = FALSE)
}

out <- file.path(cfg$paths$interim, "indicators_long.csv")
write_csv(long, out, na = "")

summary_tbl <- count(long, indicator_id, geo_type, name = "rows")
print(as.data.frame(summary_tbl), row.names = FALSE)
message("Wrote ", nrow(long), " rows to ", out)
