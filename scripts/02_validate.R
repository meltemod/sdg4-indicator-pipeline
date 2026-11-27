# Step 2: select each view's data, run consistency checks, flag problems.
#
#   Rscript scripts/02_validate.R
#
# Data is FLAGGED, never dropped or altered. Every check adds rows to the flag
# table; step 03 colours the matching cells and lists the flags in the
# workbook, so a reviewer sees exactly what was raised and why.
#
# Inputs : data/interim/indicators_long.csv (step 01), config.yml
# Outputs: data/processed/<view>.csv   one row per geo x indicator (x year);
#                                      value is NA where data is missing
#          data/interim/flags.csv      view, indicator_id, geo_code, year,
#                                      check, severity, detail
#
# Severities: error (value is wrong as stored), warning (plausible but worth
# a look), info (UIS metadata note), missing (no value).
#
# What happens, for each view in config.yml:
#   1. Keep only that view's indicators, geography and year range.
#   2. Build the full list of cells the view SHOULD have, and fill in the
#      values we DO have. Gaps become rows with value = NA.
#   3. Run the checks; each one returns the rows that fail it as flags.
#   4. Save the view's table, and collect its flags.
# Finally, all views' flags are written to one file.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

if (!file.exists("config.yml")) stop("Run from the project root.", call. = FALSE)
cfg  <- yaml::read_yaml("config.yml")
vcfg <- cfg$validation          # thresholds and colours from the validation: block

# Read step 01's output with explicit column types, so year stays a whole
# number and value stays numeric.
long <- read_csv(file.path(cfg$paths$interim, "indicators_long.csv"),
                 col_types = cols(year = col_integer(), value = col_double(),
                                  .default = col_character()),
                 na = "")
# An empty magnitude/qualifier comes back from the CSV as NA; turn it back
# into "" (as written in step 01) so text comparisons below work.
long <- mutate(long, across(c(magnitude, qualifier), ~ coalesce(.x, "")))

# Work out each indicator's unit from the end of its UIS label, e.g.
#   "...both sexes (%)"                         -> percent
#   "...15+ years, both sexes (number)"         -> count
#   "...adjusted location parity index (LPIA)"  -> parity
# The unit decides which range check applies here, and which number format
# step 03 uses. Because it is read from the label, a new indicator gets the
# right checks without code changes. case_when() tests the rules top to
# bottom and uses the first one that matches.
unit_of <- function(label) {
  case_when(
    grepl("parity index", label, ignore.case = TRUE) ~ "parity",
    grepl("\\(%\\)\\s*$", label)                    ~ "percent",
    grepl("\\(number\\)\\s*$", label)               ~ "count",
    TRUE                                            ~ "other"
  )
}
long <- mutate(long, unit = unit_of(indicator_label))

# Turn the rows that failed a check into flag rows, all in the same layout.
#   d        the failing rows (any table with indicator_id, geo_code,
#            geo_name, year columns)
#   check    short machine-readable name, e.g. "range_0_100"
#   severity error / warning / missing / info
#   detail   human-readable explanation: one text for all rows, or one per row
# Returns NULL when nothing failed, which bind_rows() later simply ignores.
flag <- function(d, check, severity, detail) {
  if (nrow(d) == 0L) return(NULL)
  tibble(indicator_id = d$indicator_id, geo_code = d$geo_code,
         geo_name = d$geo_name, year = d$year,
         check = check, severity = severity, detail = detail)
}

dir.create(cfg$paths$processed, recursive = TRUE, showWarnings = FALSE)
all_flags <- list()     # one table of flags per view, combined at the end

for (v in names(cfg$views)) {
  view  <- cfg$views[[v]]
  inds  <- unlist(view$indicators)
  yrs   <- view$years                      # list(from, to, pick)
  geo_t <- if (identical(view$geo, "countries")) "country" else "region"

  # ---- 1. this view's slice of the data -----------------------------------
  d <- long |>
    filter(indicator_id %in% inds, geo_type == geo_t,
           year >= yrs$from, year <= yrs$to)
  if (geo_t == "region") d <- filter(d, geo_code %in% unlist(view$geo$regions))

  # One row per indicator with its label and unit. It is taken from the whole
  # dataset (not the filtered slice) so an indicator still has a label even
  # if it has no values at all in this view's years.
  meta <- distinct(filter(long, indicator_id %in% inds),
                   indicator_id, indicator_label, unit)

  f <- list()   # this view's flags; each check below adds one entry

  # ---- check: duplicates ---------------------------------------------------
  # UIS should give exactly one value per indicator / place / year. Count
  # rows per key and flag any key that appears more than once. This runs on
  # the raw slice, before step 2 below keeps just one of the duplicate rows.
  dups <- d |> count(indicator_id, geo_code, geo_name, year) |> filter(n > 1)
  f$dup <- flag(dups, "duplicate", "error",
                sprintf("%d rows for the same indicator/geo/year", dups$n))

  # ---- 2. the cells the view should have, filled with what we have --------
  # The idea: first list every cell the workbook should contain (the "grid"),
  # then attach the data we found. Any cell with no data keeps value = NA, so
  # it appears in the table and gets a "missing" flag instead of silently not
  # being there.

  # Which places go in the grid?
  #   Countries: every country with at least one value for ANY of the view's
  #   indicators. A country that reports primary but not upper secondary
  #   then shows up as missing for upper secondary. A country that reports
  #   nothing at all for this view is left out, so the table isn't padded
  #   with territories that never report.
  #   Regions: exactly the regions listed in config.yml, whether or not they
  #   have data.
  geos <- distinct(filter(d, !is.na(value)), geo_code, geo_name)
  if (geo_t == "region") {
    geos <- tibble(geo_code = unlist(view$geo$regions),
                   geo_name = sub("^[^:]+:\\s*", "", unlist(view$geo$regions)))
  }

  if (identical(yrs$pick, "latest")) {
    # "latest": ONE value per country and indicator, the most recent year
    # that has a value. Countries report in different years, so each keeps
    # its own year, which step 03 shows next to the value.
    # with_ties = FALSE: if a year is duplicated, keep one row; the duplicate
    # check above has already flagged it.
    picked <- d |>
      filter(!is.na(value)) |>
      group_by(indicator_id, geo_code) |>
      slice_max(year, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(-geo_name)     # the name comes from `geos`; avoid a duplicate column in the join
    # crossing() = every combination: each place x each indicator.
    grid <- tidyr::crossing(geos, indicator_id = inds)
  } else {
    # "all": every year in the range (a time series). If a year is
    # duplicated, keep the first row; the duplicate check has already
    # flagged it.
    picked <- d |> select(-geo_name) |> distinct(indicator_id, geo_code, year, .keep_all = TRUE)
    # Every place x every indicator x every year from..to.
    grid <- tidyr::crossing(geos, indicator_id = inds, year = seq(yrs$from, yrs$to))
  }

  out <- grid |>
    # Attach values to the grid. Join on indicator + place, plus year when
    # the grid has a year column ("all" mode). In "latest" mode the year
    # comes from `picked`. Grid cells with no match keep value = NA.
    left_join(picked, by = intersect(c("indicator_id", "geo_code", "year"), names(grid))) |>
    # Rows with no data have no label or unit either. Drop those columns and
    # re-attach them from `meta`, so every row has its indicator's label and
    # unit (the range checks below need the unit even for missing rows).
    select(-any_of(c("indicator_label", "unit"))) |>
    left_join(meta, by = "indicator_id") |>
    mutate(view = v, geo_type = geo_t,
           magnitude = coalesce(magnitude, ""), qualifier = coalesce(qualifier, "")) |>
    select(view, indicator_id, indicator_label, unit, geo_type, geo_code, geo_name,
           year, value, magnitude, qualifier) |>
    arrange(indicator_id, geo_name, year)

  # ---- 3. checks on the view's table ---------------------------------------
  # Every check below follows the same pattern: filter() keeps the rows that
  # FAIL the rule, and flag() turns them into flag rows.

  # check: missing. The grid cells that found no data.
  miss <- filter(out, is.na(value))
  f$missing <- flag(miss, "missing", "missing",
                    if (identical(yrs$pick, "latest"))
                      sprintf("No value %d-%d", yrs$from, yrs$to) else "No value for this year")

  # check: rates must be between 0 and 100.
  pct_bad <- filter(out, unit == "percent", !is.na(value), value < 0 | value > 100)
  f$pct <- flag(pct_bad, "range_0_100", "error",
                sprintf("Rate %.2f is outside 0-100", pct_bad$value))

  # check: adjusted parity indices must lie within [0, 2]. UIS's adjustment
  # guarantees these bounds, so anything outside them is a data error.
  # Low values inside the bounds are real inequality and are NOT flagged
  # (see README, decision 5).
  lo <- vcfg$parity_index_bounds[[1]]; hi <- vcfg$parity_index_bounds[[2]]
  par_bad <- filter(out, unit == "parity", !is.na(value), value < lo | value > hi)
  f$par <- flag(par_bad, "parity_out_of_bounds", "error",
                sprintf("Adjusted parity index %.3f outside [%s, %s]", par_bad$value, lo, hi))

  # check: counts (e.g. number of people) can't be negative or fractional.
  # The 1e-6 tolerance ignores tiny rounding noise from storing numbers as
  # decimals, so 373042272.0000001 still counts as whole.
  cnt_bad <- filter(out, unit == "count", !is.na(value),
                    value < 0 | abs(value - round(value)) > 1e-6)
  f$cnt <- flag(cnt_bad, "count_invalid", "error",
                sprintf("Count %s is negative or not a whole number", format(cnt_bad$value)))

  # check: sudden year-on-year jumps. This applies only to time series
  # ("all"); in "latest" mode each place has a single value. For each
  # indicator and place: sort by year, compare each value with the one
  # before it (lag), and flag changes bigger than max_yoy_change_pct.
  # `year - prev_year == 1` limits this to consecutive years, so a gap
  # (e.g. 2012 -> 2015) isn't reported as a jump.
  if (!identical(yrs$pick, "latest")) {
    yoy <- out |>
      filter(!is.na(value)) |>
      group_by(indicator_id, geo_code) |>
      arrange(year, .by_group = TRUE) |>
      mutate(prev = lag(value), prev_year = lag(year),
             pct = 100 * (value - prev) / abs(prev)) |>
      ungroup() |>
      filter(!is.na(pct), year - prev_year == 1, abs(pct) > vcfg$max_yoy_change_pct)
    f$yoy <- flag(yoy, "yoy_jump", "warning",
                  sprintf("%+.1f%% vs %d", yoy$pct, yoy$prev_year))
  }

  # check: notes UIS itself attached to a value (the "magnitude" column).
  # Not errors, but useful for a reviewer to see:
  #   LOWREL  = UIS considers the value low reliability
  #   NIL, NA = the value is 0 by convention (nil, or not applicable)
  lowrel <- filter(out, magnitude == "LOWREL")
  f$lowrel <- flag(lowrel, "low_reliability", "info", "UIS marks this value as low reliability")
  nilna <- filter(out, magnitude %in% c("NIL", "NA"))
  f$nilna <- flag(nilna, "nil_or_na", "info",
                  sprintf("UIS magnitude %s: value is 0 by convention", nilna$magnitude))

  # ---- 4. save this view ---------------------------------------------------
  # Stack this view's flags (checks with no failures returned NULL and simply
  # drop out) and tag them with the view name.
  vf <- bind_rows(f)
  if (nrow(vf) > 0L) vf$view <- v
  all_flags[[v]] <- vf

  write_csv(out, file.path(cfg$paths$processed, paste0(v, ".csv")), na = "")

  # One summary line per view, e.g.
  #   out_of_school  603 rows, 587 with values | flags: missing=16
  message(sprintf("%-18s %5d rows, %4d with values | flags: %s", v, nrow(out),
                  sum(!is.na(out$value)),
                  if (nrow(vf)) paste(names(table(vf$check)), table(vf$check),
                                      sep = "=", collapse = ", ") else "none"))
}

# ---- all views' flags -> one file --------------------------------------------
flags <- bind_rows(all_flags)
# If no view raised any flag, write an empty file that still has the right
# columns, so step 03 can read it the same way every time.
if (nrow(flags) == 0L) {
  flags <- tibble(view = character(), indicator_id = character(), geo_code = character(),
                  geo_name = character(), year = integer(), check = character(),
                  severity = character(), detail = character())
}
# Most serious first within each view: error, warning, missing, info. Using a
# factor makes arrange() sort by that order rather than alphabetically.
flags <- flags |>
  select(view, indicator_id, geo_code, geo_name, year, check, severity, detail) |>
  arrange(view, factor(severity, c("error", "warning", "missing", "info")),
          indicator_id, geo_name, year)
write_csv(flags, file.path(cfg$paths$interim, "flags.csv"), na = "")
message("Wrote ", nrow(flags), " flags to ", file.path(cfg$paths$interim, "flags.csv"))
