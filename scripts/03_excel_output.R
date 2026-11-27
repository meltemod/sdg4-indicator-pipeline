# Step 3: build one formatted Excel workbook per view, with a native chart.
#
#   Rscript scripts/03_excel_output.R
#
# Inputs : data/processed/<view>.csv, data/interim/flags.csv (step 02), config.yml
# Outputs: outputs/tables/<view>.xlsx with sheets
#   Chart  the chart table (sorted, as plotted) + a native Excel chart that
#          reads those cells. Edit a number and the chart follows.
#   Data   every indicator in the view; headers built from UIS labels;
#          number formats by unit; flagged cells coloured.
#   Flags  every flag raised in step 02 for this view.
#   About  data release, run time, git commit, config hash, colour legend.
#
# The workbook is a BUILD OUTPUT. Change titles, sorting, colours, the
# charted indicator, countries/regions or years in config.yml and re-run;
# manual edits to the file are overwritten on the next run.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(openxlsx2)
  library(mschart)
  library(officer)   # fp_text() for chart fonts
})

if (!file.exists("config.yml")) stop("Run from the project root.", call. = FALSE)
cfg <- yaml::read_yaml("config.yml")
out_dir <- file.path(cfg$paths$outputs, "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

flags_all <- read_csv(file.path(cfg$paths$interim, "flags.csv"),
                      col_types = cols(year = col_integer(), .default = col_character()),
                      na = "")

# ---- helpers ---------------------------------------------------------------
numfmt_for <- function(unit) {
  switch(unit, percent = "0.0", parity = "0.00", count = "#,##0", "General")
}

# Short axis title by unit; the full UIS label is already in the chart title
# and the table header. Override per view with chart.y_label in config.yml.
axis_label_for <- function(unit) {
  switch(unit, percent = "%", parity = "Parity index", count = "Number", "")
}

# Consistent, modest chart typography.
apply_theme <- function(g, legend) {
  chart_theme(g,
    main_title   = fp_text(font.size = 14, bold = TRUE),
    axis_title_x = fp_text(font.size = 10),
    axis_title_y = fp_text(font.size = 10),
    axis_text_x  = fp_text(font.size = 8),
    axis_text_y  = fp_text(font.size = 9),
    legend_text  = fp_text(font.size = 10),
    legend_position = legend)
}

argb <- function(hex) wb_color(hex = paste0("FF", toupper(sub("^#", "", hex))))
flag_colors <- vapply(cfg$validation$flag_colors, identity, character(1))
HEADER_FILL <- "#1F3864"

# Run provenance, written to every About sheet.
git_commit <- tryCatch({
  sha   <- system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)
  dirty <- length(system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE)) > 0
  paste0(sha, if (dirty) " (uncommitted changes present)" else "")
}, error = function(e) "unknown", warning = function(w) "unknown")
config_md5 <- unname(tools::md5sum("config.yml"))
generated  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

style_header <- function(wb, sheet, dims) {
  wb$add_fill(sheet, dims = dims, color = argb(HEADER_FILL))
  wb$add_font(sheet, dims = dims, bold = TRUE, color = wb_color(hex = "FFFFFFFF"))
  wb$add_cell_style(sheet, dims = dims, wrap_text = TRUE,
                    vertical = "center", horizontal = "center")
}

# ---- one workbook per view -------------------------------------------------
for (v in names(cfg$views)) {
  view <- cfg$views[[v]]
  d <- read_csv(file.path(cfg$paths$processed, paste0(v, ".csv")),
                col_types = cols(year = col_integer(), value = col_double(),
                                 .default = col_character()),
                na = "") |>
    mutate(across(c(magnitude, qualifier), ~ coalesce(.x, "")))
  fl <- filter(flags_all, view == v)

  inds    <- unlist(view$indicators)
  meta    <- distinct(d, indicator_id, indicator_label, unit) |>
    arrange(match(indicator_id, inds))
  latest  <- identical(view$years$pick, "latest")
  geo_lbl <- if (identical(view$geo, "countries")) "Country" else "Region"

  wb <- wb_workbook(creator = "sdg4-indicator-pipeline", title = view$title)
  wb$add_worksheet("Chart", grid_lines = FALSE)
  wb$add_worksheet("Data")
  wb$add_worksheet("Flags")
  wb$add_worksheet("About", grid_lines = FALSE)

  # ======================= Data sheet ======================================
  # Two header rows: row 1 = indicator label (merged over its columns),
  # row 2 = sub-column. Keys (geo, code[, year]) span both rows.
  if (latest) {
    keys     <- distinct(d, geo_code, geo_name) |> arrange(geo_name)
    sub_cols <- c("Value", "Year", "UIS note")
    key_hdr  <- c(geo_lbl, "Code")
    wide     <- keys
    for (i in meta$indicator_id) {
      di <- filter(d, indicator_id == i) |>
        transmute(geo_code, !!paste0(i, "|v") := value, !!paste0(i, "|y") := year,
                  !!paste0(i, "|n") := trimws(paste(magnitude, qualifier)))
      wide <- left_join(wide, di, by = "geo_code")
    }
    wide <- relocate(wide, geo_name, geo_code)
  } else {
    keys     <- distinct(d, geo_code, geo_name, year) |> arrange(geo_name, year)
    sub_cols <- c("Value", "UIS note")
    key_hdr  <- c(geo_lbl, "Year")
    wide     <- keys
    for (i in meta$indicator_id) {
      di <- filter(d, indicator_id == i) |>
        transmute(geo_code, year, !!paste0(i, "|v") := value,
                  !!paste0(i, "|n") := trimws(paste(magnitude, qualifier)))
      wide <- left_join(wide, di, by = c("geo_code", "year"))
    }
    wide <- select(wide, geo_name, year, everything(), -geo_code) |>
      mutate(geo_code = keys$geo_code)            # kept for flag lookup, not written
  }

  n_key <- length(key_hdr); n_sub <- length(sub_cols)
  body_out <- if (latest) wide else select(wide, -geo_code)
  first_row <- 3L
  last_row  <- first_row + nrow(body_out) - 1L

  wb$add_data("Data", body_out, dims = wb_dims(first_row, 1), col_names = FALSE, na.strings = "")

  for (k in seq_len(n_key)) {
    wb$add_data("Data", key_hdr[k], dims = wb_dims(1, k))
    wb$merge_cells("Data", dims = wb_dims(1:2, k))
  }
  for (j in seq_len(nrow(meta))) {
    c0 <- n_key + (j - 1L) * n_sub + 1L
    cols_j <- c0:(c0 + n_sub - 1L)
    wb$add_data("Data", meta$indicator_label[j], dims = wb_dims(1, c0))
    wb$merge_cells("Data", dims = wb_dims(1, cols_j))
    wb$add_data("Data", t(sub_cols), dims = wb_dims(2, c0), col_names = FALSE)
    wb$add_numfmt("Data", dims = wb_dims(first_row:last_row, c0),
                  numfmt = numfmt_for(meta$unit[j]))
    wb$set_col_widths("Data", cols = cols_j, widths = c(11, 7, 10)[seq_len(n_sub)])
  }
  n_col <- n_key + nrow(meta) * n_sub
  style_header(wb, "Data", wb_dims(1:2, 1:n_col))
  wb$set_row_heights("Data", rows = 1, heights = 60)
  wb$set_col_widths("Data", cols = 1:n_key, widths = c(32, 8)[seq_len(n_key)])
  wb$freeze_pane("Data", first_active_row = first_row, first_active_col = n_key + 1L)
  wb$add_filter("Data", rows = 2, cols = 1:n_col)

  # Colour flagged value cells. Most severe flag wins when a cell has several.
  if (nrow(fl) > 0L) {
    sev_rank <- c(error = 1, warning = 2, missing = 3, info = 4)
    cell_fl <- fl |>
      mutate(rank = sev_rank[severity]) |>
      group_by(indicator_id, geo_code, year = if (latest) NA_integer_ else year) |>
      slice_min(rank, n = 1, with_ties = FALSE) |>
      ungroup()
    row_of <- if (latest) match(cell_fl$geo_code, wide$geo_code) else
      match(paste(cell_fl$geo_code, cell_fl$year), paste(wide$geo_code, wide$year))
    col_of <- n_key + (match(cell_fl$indicator_id, meta$indicator_id) - 1L) * n_sub + 1L
    for (r in which(!is.na(row_of) & !is.na(col_of))) {
      wb$add_fill("Data", dims = wb_dims(first_row + row_of[r] - 1L, col_of[r]),
                  color = argb(flag_colors[[cell_fl$severity[r]]]))
    }
  }

  # ======================= Chart sheet =====================================
  ch   <- view$chart
  ci   <- ch$indicator
  cmet <- filter(meta, indicator_id == ci)
  if (nrow(cmet) == 0L) stop(v, ": chart.indicator ", ci, " is not in the view's indicators")

  wb$add_data("Chart", ch$title %||% view$title, dims = "A1")
  wb$add_font("Chart", dims = "A1", bold = TRUE, size = 14)
  wb$add_data("Chart", sprintf("Source: UIS bulk data, release %s. Chart reads the table below; change config.yml and re-run to alter it.",
                               cfg$bdds$release), dims = "A2")
  wb$add_font("Chart", dims = "A2", italic = TRUE, color = wb_color(hex = "FF595959"))

  if (ch$type == "bar") {
    cd <- filter(d, indicator_id == ci, !is.na(value)) |>
      transmute(!!geo_lbl := geo_name, Value = value, Year = year)
    cd <- switch(ch$sort %||% "none",
                 desc = arrange(cd, desc(Value)),
                 asc  = arrange(cd, Value),
                 cd)
    tbl_row <- 4L
    wb$add_data("Chart", cd, dims = wb_dims(tbl_row, 1))
    style_header(wb, "Chart", wb_dims(tbl_row, 1:3))
    wb$add_numfmt("Chart", dims = wb_dims(tbl_row + seq_len(nrow(cd)), 2),
                  numfmt = numfmt_for(cmet$unit))
    wb$set_col_widths("Chart", cols = 1:3, widths = c(30, 10, 7))

    dat <- wb_data(wb, "Chart", dims = wb_dims(tbl_row + 0:nrow(cd), 1:2))
    g <- ms_barchart(dat, x = geo_lbl, y = "Value")
    g <- chart_settings(g, dir = "vertical", gap_width = 40)
    if (!is.null(ch$color)) {
      g <- chart_data_fill(g, values = c(Value = ch$color))
      g <- chart_data_stroke(g, values = c(Value = ch$color))
    }
    g <- chart_labels(g, title = ch$title, xlab = NULL,
                      ylab = ch$y_label %||% axis_label_for(cmet$unit))
    g <- chart_ax_x(g, rotation = -90)
    g <- chart_ax_y(g, num_fmt = numfmt_for(cmet$unit))
    g <- apply_theme(g, legend = "n")
    # Wide enough that every country label fits.
    width_cols <- max(15L, ceiling(nrow(cd) / 5))
    wb$add_mschart("Chart", dims = paste0(wb_dims(tbl_row, 5), ":",
                                          wb_dims(tbl_row + 34, 4 + width_cols)),
                   graph = g)
  } else {
    cd <- filter(d, indicator_id == ci) |>
      select(geo_name, year, value) |>
      pivot_wider(names_from = geo_name, values_from = value) |>
      arrange(year) |>
      rename(Year = year)
    regs <- setdiff(names(cd), "Year")
    tbl_row <- 4L
    wb$add_data("Chart", cd, dims = wb_dims(tbl_row, 1))
    style_header(wb, "Chart", wb_dims(tbl_row, seq_along(cd)))
    wb$add_numfmt("Chart", dims = wb_dims(tbl_row + seq_len(nrow(cd)), 1 + seq_along(regs)),
                  numfmt = numfmt_for(cmet$unit))
    wb$set_col_widths("Chart", cols = seq_along(cd), widths = c(7, rep(16, length(regs))))

    dat <- wb_data(wb, "Chart", dims = wb_dims(tbl_row + 0:nrow(cd), seq_along(cd)))
    g <- ms_linechart(dat, x = "Year", y = regs)
    g <- chart_settings(g, style = "lineMarker")
    g <- chart_labels(g, title = ch$title, xlab = NULL,
                      ylab = ch$y_label %||% axis_label_for(cmet$unit))
    g <- chart_ax_x(g, num_fmt = "0")
    g <- chart_ax_y(g, num_fmt = numfmt_for(cmet$unit))
    g <- apply_theme(g, legend = "b")
    c0 <- length(cd) + 2L
    wb$add_mschart("Chart", dims = paste0(wb_dims(tbl_row, c0), ":", wb_dims(tbl_row + 24, c0 + 9)),
                   graph = g)
  }

  # Print the Chart sheet landscape. Line charts fit on one page; long
  # country tables fit the page width and run onto further pages.
  if (ch$type == "line") {
    wb$page_setup("Chart", orientation = "landscape", fit_to_width = 1, fit_to_height = 1)
  } else {
    wb$page_setup("Chart", orientation = "landscape", fit_to_width = 1, fit_to_height = 0)
  }

  # ======================= Flags sheet =====================================
  fl_out <- fl |>
    left_join(select(meta, indicator_id, indicator_label), by = "indicator_id") |>
    transmute(Severity = severity, Check = check, Indicator = indicator_id,
              `Indicator label` = indicator_label, !!geo_lbl := geo_name,
              Code = geo_code, Year = year, Detail = detail)
  if (nrow(fl_out) == 0L) fl_out <- tibble(Severity = "none", Detail = "No flags raised.")
  wb$add_data("Flags", fl_out)
  style_header(wb, "Flags", wb_dims(1, seq_along(fl_out)))
  if (ncol(fl_out) > 2L) {
    wb$set_col_widths("Flags", cols = seq_along(fl_out), widths = c(10, 20, 15, 45, 28, 9, 7, 40))
    for (s in names(flag_colors)) {
      rows <- which(fl_out$Severity == s) + 1L
      if (length(rows)) {
        for (r in rows) wb$add_fill("Flags", dims = wb_dims(r, 1), color = argb(flag_colors[[s]]))
      }
    }
    wb$add_filter("Flags", rows = 1, cols = seq_along(fl_out))
  }
  wb$freeze_pane("Flags", first_row = TRUE)

  # ======================= About sheet =====================================
  about <- tibble::tribble(
    ~Field, ~Value,
    "Workbook", view$title,
    "STATUS", "Generated file. Request changes via config.yml (views: section) and re-run; manual edits are overwritten on the next run.",
    "Data Browser view", view$source_url,
    "Source", sprintf("UIS Bulk Data Download Service, release %s (%s/%s/)", cfg$bdds$release, cfg$bdds$base_url, cfg$bdds$release),
    "Indicators", paste(sprintf("%s: %s", meta$indicator_id, meta$indicator_label), collapse = "\n"),
    "Geography", if (identical(view$geo, "countries")) "All countries with data" else paste(unlist(view$geo$regions), collapse = "; "),
    "Years", sprintf("%d-%d, %s", view$years$from, view$years$to,
                     if (latest) "latest available value per country" else "every year"),
    "Generated at", generated,
    "Git commit", git_commit,
    "config.yml MD5", config_md5,
    "Rows (Data sheet)", as.character(nrow(body_out)),
    "Flags", if (nrow(fl)) paste(names(table(fl$severity)), table(fl$severity), sep = ": ", collapse = ", ") else "none",
    "UIS note column", "Magnitude/qualifier from UIS: UIS_EST = UIS estimate, NAT_EST = national estimate, LOWREL = low reliability, NIL/NA = zero by convention."
  )
  wb$add_data("About", about)
  style_header(wb, "About", "A1:B1")
  wb$add_font("About", dims = "A2:A14", bold = TRUE)
  wb$add_cell_style("About", dims = paste0("B2:B", nrow(about) + 1), wrap_text = TRUE, vertical = "top")
  wb$add_cell_style("About", dims = paste0("A2:A", nrow(about) + 1), vertical = "top")
  wb$add_fill("About", dims = "A3:B3", color = argb(flag_colors[["warning"]]))
  wb$set_col_widths("About", cols = 1:2, widths = c(22, 110))

  legend_row <- nrow(about) + 3L
  wb$add_data("About", "Cell colours in the Data sheet", dims = wb_dims(legend_row, 1))
  wb$add_font("About", dims = wb_dims(legend_row, 1), bold = TRUE)
  legend <- c(error = "Error: value fails a consistency rule",
              warning = "Warning: plausible but worth checking",
              missing = "Missing: no value for this cell",
              info = "Info: UIS metadata note (e.g. low reliability)")
  for (k in seq_along(legend)) {
    r <- legend_row + k
    wb$add_fill("About", dims = wb_dims(r, 1), color = argb(flag_colors[[names(legend)[k]]]))
    wb$add_data("About", names(legend)[k], dims = wb_dims(r, 1))
    wb$add_data("About", legend[[k]], dims = wb_dims(r, 2))
  }

  path <- file.path(out_dir, paste0(v, ".xlsx"))
  wb$save(path, overwrite = TRUE)
  message(sprintf("Wrote %s (%d data rows, %d chart-table rows, %d flags)",
                  path, nrow(body_out), nrow(cd), nrow(fl)))
}
