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

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

if (!file.exists("config.yml")) stop("Run from the project root.", call. = FALSE)
cfg  <- yaml::read_yaml("config.yml")
vcfg <- cfg$validation

long <- read_csv(file.path(cfg$paths$interim, "indicators_long.csv"),
                 col_types = cols(year = col_integer(), value = col_double(),
                                  .default = col_character()),
                 na = "")
long <- mutate(long, across(c(magnitude, qualifier), ~ coalesce(.x, "")))

# Unit from the UIS label: "(%)" rate, "(number)" count, "parity index".
unit_of <- function(label) {
  case_when(
    grepl("parity index", label, ignore.case = TRUE) ~ "parity",
    grepl("\\(%\\)\\s*$", label)                    ~ "percent",
    grepl("\\(number\\)\\s*$", label)               ~ "count",
    TRUE                                            ~ "other"
  )
}
long <- mutate(long, unit = unit_of(indicator_label))

flag <- function(d, check, severity, detail) {
  if (nrow(d) == 0L) return(NULL)
  tibble(indicator_id = d$indicator_id, geo_code = d$geo_code,
         geo_name = d$geo_name, year = d$year,
         check = check, severity = severity, detail = detail)
}

dir.create(cfg$paths$processed, recursive = TRUE, showWarnings = FALSE)
all_flags <- list()

for (v in names(cfg$views)) {
  view  <- cfg$views[[v]]
  inds  <- unlist(view$indicators)
  yrs   <- view$years
  geo_t <- if (identical(view$geo, "countries")) "country" else "region"

  d <- long |>
    filter(indicator_id %in% inds, geo_type == geo_t,
           year >= yrs$from, year <= yrs$to)
  if (geo_t == "region") d <- filter(d, geo_code %in% unlist(view$geo$regions))

  meta <- distinct(filter(long, indicator_id %in% inds),
                   indicator_id, indicator_label, unit)
  f <- list()

  # -- duplicates: more than one row for the same key ----------------------
  dups <- d |> count(indicator_id, geo_code, geo_name, year) |> filter(n > 1)
  f$dup <- flag(dups, "duplicate", "error",
                sprintf("%d rows for the same indicator/geo/year", dups$n))

  # -- select years ---------------------------------------------------------
  # The geo universe is every geo with at least one value for ANY of the
  # view's indicators in the year range, so a country present for one level
  # but absent for another shows up as missing, not silently omitted.
  geos <- distinct(filter(d, !is.na(value)), geo_code, geo_name)
  if (geo_t == "region") {
    geos <- tibble(geo_code = unlist(view$geo$regions),
                   geo_name = sub("^[^:]+:\\s*", "", unlist(view$geo$regions)))
  }

  if (identical(yrs$pick, "latest")) {
    picked <- d |>
      filter(!is.na(value)) |>
      group_by(indicator_id, geo_code) |>
      slice_max(year, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(-geo_name)
    grid <- tidyr::crossing(geos, indicator_id = inds)
  } else {
    picked <- d |> select(-geo_name) |> distinct(indicator_id, geo_code, year, .keep_all = TRUE)
    grid <- tidyr::crossing(geos, indicator_id = inds, year = seq(yrs$from, yrs$to))
  }

  out <- grid |>
    left_join(picked, by = intersect(c("indicator_id", "geo_code", "year"), names(grid))) |>
    select(-any_of(c("indicator_label", "unit"))) |>
    left_join(meta, by = "indicator_id") |>
    mutate(view = v, geo_type = geo_t,
           magnitude = coalesce(magnitude, ""), qualifier = coalesce(qualifier, "")) |>
    select(view, indicator_id, indicator_label, unit, geo_type, geo_code, geo_name,
           year, value, magnitude, qualifier) |>
    arrange(indicator_id, geo_name, year)

  # -- missing --------------------------------------------------------------
  miss <- filter(out, is.na(value))
  f$missing <- flag(miss, "missing", "missing",
                    if (identical(yrs$pick, "latest"))
                      sprintf("No value %d-%d", yrs$from, yrs$to) else "No value for this year")

  # -- range checks by unit -------------------------------------------------
  pct_bad <- filter(out, unit == "percent", !is.na(value), value < 0 | value > 100)
  f$pct <- flag(pct_bad, "range_0_100", "error",
                sprintf("Rate %.2f is outside 0-100", pct_bad$value))

  lo <- vcfg$parity_index_bounds[[1]]; hi <- vcfg$parity_index_bounds[[2]]
  par_bad <- filter(out, unit == "parity", !is.na(value), value < lo | value > hi)
  f$par <- flag(par_bad, "parity_out_of_bounds", "error",
                sprintf("Adjusted parity index %.3f outside [%s, %s]", par_bad$value, lo, hi))

  cnt_bad <- filter(out, unit == "count", !is.na(value),
                    value < 0 | abs(value - round(value)) > 1e-6)
  f$cnt <- flag(cnt_bad, "count_invalid", "error",
                sprintf("Count %s is negative or not a whole number", format(cnt_bad$value)))

  # -- year-on-year jumps (time series only) --------------------------------
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

  # -- UIS metadata notes ---------------------------------------------------
  lowrel <- filter(out, magnitude == "LOWREL")
  f$lowrel <- flag(lowrel, "low_reliability", "info", "UIS marks this value as low reliability")
  nilna <- filter(out, magnitude %in% c("NIL", "NA"))
  f$nilna <- flag(nilna, "nil_or_na", "info",
                  sprintf("UIS magnitude %s: value is 0 by convention", nilna$magnitude))

  vf <- bind_rows(f)
  if (nrow(vf) > 0L) vf$view <- v
  all_flags[[v]] <- vf

  write_csv(out, file.path(cfg$paths$processed, paste0(v, ".csv")), na = "")
  message(sprintf("%-18s %5d rows, %4d with values | flags: %s", v, nrow(out),
                  sum(!is.na(out$value)),
                  if (nrow(vf)) paste(names(table(vf$check)), table(vf$check),
                                      sep = "=", collapse = ", ") else "none"))
}

flags <- bind_rows(all_flags)
if (nrow(flags) == 0L) {
  flags <- tibble(view = character(), indicator_id = character(), geo_code = character(),
                  geo_name = character(), year = integer(), check = character(),
                  severity = character(), detail = character())
}
flags <- flags |>
  select(view, indicator_id, geo_code, geo_name, year, check, severity, detail) |>
  arrange(view, factor(severity, c("error", "warning", "missing", "info")),
          indicator_id, geo_name, year)
write_csv(flags, file.path(cfg$paths$interim, "flags.csv"), na = "")
message("Wrote ", nrow(flags), " flags to ", file.path(cfg$paths$interim, "flags.csv"))
