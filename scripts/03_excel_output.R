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
#
# How openxlsx2 is used here, in brief:
#   wb <- wb_workbook()            an empty workbook held in memory
#   wb$add_worksheet("Data")       add a sheet
#   wb$add_data(sheet, x, dims)    write a value or table starting at a cell
#   wb$add_fill / add_font / ...   style a range of cells
#   wb$save(path)                  write the .xlsx file
# Cell positions ("dims") are built with wb_dims(row, col), e.g.
#   wb_dims(3, 1)      -> "A3"
#   wb_dims(1:2, 1:3)  -> "A1:C2"
# so the code works with row/column numbers instead of spreadsheet letters.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(openxlsx2)   # writes the .xlsx file
  library(mschart)     # describes native Excel charts for openxlsx2 to embed
  library(officer)     # fp_text(): font settings for chart text
})

if (!file.exists("config.yml")) stop("Run from the project root.", call. = FALSE)
cfg <- yaml::read_yaml("config.yml")
out_dir <- file.path(cfg$paths$outputs, "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# All flags from step 02, for every view. Each workbook takes its own subset.
flags_all <- read_csv(file.path(cfg$paths$interim, "flags.csv"),
                      col_types = cols(year = col_integer(), .default = col_character()),
                      na = "")

# ---- helpers ---------------------------------------------------------------

# Excel number format for each unit (step 02 derived the unit from the UIS
# label). This changes only how a number is DISPLAYED; the full-precision
# value stays in the cell.
#   percent 12.3456 -> "12.3"   parity 0.96712 -> "0.97"   count 1234567 -> "1,234,567"
numfmt_for <- function(unit) {
  switch(unit, percent = "0.0", parity = "0.00", count = "#,##0", "General")
}

# Short y-axis title by unit. The full UIS label is already in the chart
# title and the table header, and a long label wraps into an unreadable
# block beside the axis. Override per view with chart.y_label in config.yml.
axis_label_for <- function(unit) {
  switch(unit, percent = "%", parity = "Parity index", count = "Number", "")
}

# One set of font sizes for every chart, so the workbooks look consistent.
# legend: "n" = no legend (single-series bar charts), "b" = legend at the bottom.
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

# Make Excel show EVERY country name under a bar chart.
#
# The problem: when Excel thinks the category labels (country names) are too
# crowded, it quietly shows only every 2nd or 3rd one. All bars are still
# drawn, but some have no name; that is why Uruguay "disappeared".
#
# The fix: a chart inside an .xlsx file is stored as XML text. The x-axis
# section of that XML (<c:catAx> ... </c:catAx>) accepts a setting
# <c:tickLblSkip val="1"/>, which means "label every category, skip none".
# mschart can't set it, so we insert it into the XML ourselves.
#
# How: openxlsx2 keeps each chart's XML as a text string in wb$charts$chart.
# We take the chart added most recently (the last row) and replace its
# closing </c:catAx> tag with the extra settings followed by </c:catAx>.
# Excel requires the settings in a fixed order, so the neighbours of
# tickLblSkip (auto, lblAlgn, lblOffset, noMultiLvlLbl) are written too, with
# Excel's default values. The grepl() check skips charts that already have
# the setting, so calling this twice does no harm.
#
# Call it straight after wb$add_mschart() for the bar chart it should fix.
force_every_category_label <- function(wb) {
  i <- nrow(wb$charts)                 # the chart added most recently
  x <- wb$charts$chart[i]              # its XML, as one long string
  if (!grepl("<c:tickLblSkip", x, fixed = TRUE)) {
    x <- sub("</c:catAx>",
             paste0('<c:auto val="1"/><c:lblAlgn val="ctr"/><c:lblOffset val="100"/>',
                    '<c:tickLblSkip val="1"/><c:noMultiLvlLbl val="0"/></c:catAx>'),
             x, fixed = TRUE)
    wb$charts$chart[i] <- x
  }
  wb
}

# Colours in config.yml are written as "#F4B6B6". Excel wants "ARGB": an
# extra alpha (opacity) byte in front, where FF = fully opaque. So
# "#F4B6B6" becomes "FFF4B6B6".
argb <- function(hex) wb_color(hex = paste0("FF", toupper(sub("^#", "", hex))))
# The flag_colors block from config.yml as a named vector:
#   c(error = "#F4B6B6", warning = "#FCE4A8", info = "#DDEBF7", missing = "#E7E6E6")
flag_colors <- vapply(cfg$validation$flag_colors, identity, character(1))
HEADER_FILL <- "#1F3864"   # dark blue for header rows

# ---- run provenance (written to every About sheet) -------------------------
# These record exactly which code, config and time produced the file, so
# anyone holding a workbook can trace where it came from.
#   git_commit: the current commit's short id, plus a warning if there were
#               uncommitted changes (the code that ran isn't fully saved in
#               git). Falls back to "unknown" if git isn't available.
#   config_md5: a fingerprint of config.yml. Two workbooks with the same
#               fingerprint were built from identical settings.
git_commit <- tryCatch({
  sha   <- system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)
  dirty <- length(system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE)) > 0
  paste0(sha, if (dirty) " (uncommitted changes present)" else "")
}, error = function(e) "unknown", warning = function(w) "unknown")
config_md5 <- unname(tools::md5sum("config.yml"))
generated  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# The look of every header row: dark blue fill, bold white text, centred, wrapped.
style_header <- function(wb, sheet, dims) {
  wb$add_fill(sheet, dims = dims, color = argb(HEADER_FILL))
  wb$add_font(sheet, dims = dims, bold = TRUE, color = wb_color(hex = "FFFFFFFF"))
  wb$add_cell_style(sheet, dims = dims, wrap_text = TRUE,
                    vertical = "center", horizontal = "center")
}

# ---- one workbook per view -------------------------------------------------
for (v in names(cfg$views)) {
  view <- cfg$views[[v]]

  # This view's table from step 02: one row per place x indicator (x year).
  d <- read_csv(file.path(cfg$paths$processed, paste0(v, ".csv")),
                col_types = cols(year = col_integer(), value = col_double(),
                                 .default = col_character()),
                na = "") |>
    mutate(across(c(magnitude, qualifier), ~ coalesce(.x, "")))
  fl <- filter(flags_all, view == v)   # this view's flags

  inds    <- unlist(view$indicators)
  # One row per indicator (id, label, unit), in the order config.yml lists
  # them, so workbook columns follow the config rather than alphabetical order.
  meta    <- distinct(d, indicator_id, indicator_label, unit) |>
    arrange(match(indicator_id, inds))
  latest  <- identical(view$years$pick, "latest")   # TRUE: one value per place; FALSE: time series
  geo_lbl <- if (identical(view$geo, "countries")) "Country" else "Region"

  wb <- wb_workbook(creator = "sdg4-indicator-pipeline", title = view$title)
  wb$add_worksheet("Chart", grid_lines = FALSE)   # first sheet = what opens first
  wb$add_worksheet("Data")
  wb$add_worksheet("Flags")
  wb$add_worksheet("About", grid_lines = FALSE)

  # ======================= Data sheet ======================================
  # Target layout ("latest" view; a time-series view has Year instead of
  # Code, and no per-indicator Year column):
  #
  #   row 1 |         |      | <indicator 1 label>       | <indicator 2 label>       |
  #   row 2 | Country | Code | Value | Year | UIS note   | Value | Year | UIS note   |
  #   row 3 | Albania | ALB  |  0.5  | 2024 |            |  5.4  | 2023 |            |
  #
  # "Country" and "Code" each span rows 1-2 (merged vertically). Each
  # indicator label spans its sub-columns (merged horizontally).
  #
  # First, reshape the long data into one row per place (per year for time
  # series) with a column group per indicator. The column names here (e.g.
  # "ROFST.1.CP|v" for value, "|y" year, "|n" note) are only internal: the
  # data is written to Excel without them, and the real headers are written
  # separately below.
  if (latest) {
    keys     <- distinct(d, geo_code, geo_name) |> arrange(geo_name)   # one row per country
    sub_cols <- c("Value", "Year", "UIS note")
    key_hdr  <- c(geo_lbl, "Code")
    wide     <- keys
    for (i in meta$indicator_id) {
      # This indicator's columns. The UIS note joins magnitude and qualifier
      # (e.g. "LOWREL UIS_EST"); trimws() removes the space left when one is empty.
      di <- filter(d, indicator_id == i) |>
        transmute(geo_code, !!paste0(i, "|v") := value, !!paste0(i, "|y") := year,
                  !!paste0(i, "|n") := trimws(paste(magnitude, qualifier)))
      wide <- left_join(wide, di, by = "geo_code")   # add them to the right
    }
    wide <- relocate(wide, geo_name, geo_code)       # name first, then code
  } else {
    keys     <- distinct(d, geo_code, geo_name, year) |> arrange(geo_name, year)  # one row per region-year
    sub_cols <- c("Value", "UIS note")
    key_hdr  <- c(geo_lbl, "Year")
    wide     <- keys
    for (i in meta$indicator_id) {
      di <- filter(d, indicator_id == i) |>
        transmute(geo_code, year, !!paste0(i, "|v") := value,
                  !!paste0(i, "|n") := trimws(paste(magnitude, qualifier)))
      wide <- left_join(wide, di, by = c("geo_code", "year"))
    }
    # The sheet shows only the region NAME and year, not the code. So move
    # geo_code to the last column: it is dropped before writing (body_out
    # below) but kept here so flags can be matched to rows by code.
    # left_join() keeps the row order of `keys`, so keys$geo_code lines up.
    wide <- select(wide, geo_name, year, everything(), -geo_code) |>
      mutate(geo_code = keys$geo_code)
  }

  n_key <- length(key_hdr)    # columns that identify the row (2)
  n_sub <- length(sub_cols)   # columns per indicator (3 or 2)
  body_out <- if (latest) wide else select(wide, -geo_code)   # exactly what goes on the sheet
  first_row <- 3L                                   # data starts below the 2 header rows
  last_row  <- first_row + nrow(body_out) - 1L

  # Write the data block (without column names) starting at A3.
  wb$add_data("Data", body_out, dims = wb_dims(first_row, 1), col_names = FALSE, na.strings = "")

  # Header for the key columns: write the name in row 1, merge rows 1-2.
  for (k in seq_len(n_key)) {
    wb$add_data("Data", key_hdr[k], dims = wb_dims(1, k))
    wb$merge_cells("Data", dims = wb_dims(1:2, k))
  }
  # Header for each indicator's column group.
  # Indicator j's group starts at column c0 = n_key + (j - 1) * n_sub + 1:
  # with 2 key columns and 3 sub-columns, the groups start at columns
  # 3, 6, 9, ... (C, F, I, ...). Its Value column is always the first in the group.
  for (j in seq_len(nrow(meta))) {
    c0 <- n_key + (j - 1L) * n_sub + 1L
    cols_j <- c0:(c0 + n_sub - 1L)
    wb$add_data("Data", meta$indicator_label[j], dims = wb_dims(1, c0))   # UIS label, row 1
    wb$merge_cells("Data", dims = wb_dims(1, cols_j))                     # span the group
    wb$add_data("Data", t(sub_cols), dims = wb_dims(2, c0), col_names = FALSE)  # Value/Year/UIS note, row 2
    # Number format on the Value column only, chosen by this indicator's unit.
    wb$add_numfmt("Data", dims = wb_dims(first_row:last_row, c0),
                  numfmt = numfmt_for(meta$unit[j]))
    wb$set_col_widths("Data", cols = cols_j, widths = c(11, 7, 10)[seq_len(n_sub)])
  }
  n_col <- n_key + nrow(meta) * n_sub                 # total columns used
  style_header(wb, "Data", wb_dims(1:2, 1:n_col))
  wb$set_row_heights("Data", rows = 1, heights = 60)  # room for long labels to wrap
  wb$set_col_widths("Data", cols = 1:n_key, widths = c(32, 8)[seq_len(n_key)])
  # Freeze the two header rows and the key columns, so they stay visible
  # while scrolling. Add filter drop-downs on the sub-header row.
  wb$freeze_pane("Data", first_active_row = first_row, first_active_col = n_key + 1L)
  wb$add_filter("Data", rows = 2, cols = 1:n_col)

  # Colour each flagged Value cell by its flag's severity.
  if (nrow(fl) > 0L) {
    # A cell can have several flags (e.g. an error AND a UIS note). It can
    # show only one colour, so keep the most serious flag per cell. Lower
    # rank = more serious. In "latest" mode a cell is identified by
    # indicator + place only (year is set to NA), because each place has
    # one value per indicator.
    sev_rank <- c(error = 1, warning = 2, missing = 3, info = 4)
    cell_fl <- fl |>
      mutate(rank = sev_rank[severity]) |>
      group_by(indicator_id, geo_code, year = if (latest) NA_integer_ else year) |>
      slice_min(rank, n = 1, with_ties = FALSE) |>
      ungroup()

    # Locate each flag's cell on the sheet:
    #   row_of: the flag's position among the data rows, found by matching
    #           its place (and year, for time series) to `wide`.
    #   col_of: the indicator's Value column, same formula as c0 above.
    # match() returns NA when there is no match, and such flags are skipped.
    row_of <- if (latest) match(cell_fl$geo_code, wide$geo_code) else
      match(paste(cell_fl$geo_code, cell_fl$year), paste(wide$geo_code, wide$year))
    col_of <- n_key + (match(cell_fl$indicator_id, meta$indicator_id) - 1L) * n_sub + 1L
    for (r in which(!is.na(row_of) & !is.na(col_of))) {
      # row_of is a position among data rows (1 = first data row), and data
      # starts at sheet row 3, hence first_row + row_of - 1.
      wb$add_fill("Data", dims = wb_dims(first_row + row_of[r] - 1L, col_of[r]),
                  color = argb(flag_colors[[cell_fl$severity[r]]]))
    }
  }

  # ======================= Chart sheet =====================================
  # Layout: title in A1, source note in A2, then from row 4 a small table
  # with exactly what the chart plots, and the chart to its right. The chart
  # READS that table's cells, so editing a number in Excel updates the chart.
  ch   <- view$chart
  ci   <- ch$indicator
  cmet <- filter(meta, indicator_id == ci)   # label and unit of the charted indicator
  if (nrow(cmet) == 0L) stop(v, ": chart.indicator ", ci, " is not in the view's indicators")

  wb$add_data("Chart", ch$title %||% view$title, dims = "A1")   # %||%: use the view title if no chart title
  wb$add_font("Chart", dims = "A1", bold = TRUE, size = 14)
  wb$add_data("Chart", sprintf("Source: UIS bulk data, release %s. Chart reads the table below; change config.yml and re-run to alter it.",
                               cfg$bdds$release), dims = "A2")
  wb$add_font("Chart", dims = "A2", italic = TRUE, color = wb_color(hex = "FF595959"))

  if (ch$type == "bar") {
    # --- bar chart: one bar per country ---
    # Chart table: Country | Value | Year, only countries that have a value,
    # sorted as configured (chart.sort). Bars appear in table order.
    cd <- filter(d, indicator_id == ci, !is.na(value)) |>
      transmute(!!geo_lbl := geo_name, Value = value, Year = year)
    cd <- switch(ch$sort %||% "none",
                 desc = arrange(cd, desc(Value)),
                 asc  = arrange(cd, Value),
                 cd)                                   # "none": leave as is
    tbl_row <- 4L
    wb$add_data("Chart", cd, dims = wb_dims(tbl_row, 1))   # header in row 4, data from row 5
    style_header(wb, "Chart", wb_dims(tbl_row, 1:3))
    wb$add_numfmt("Chart", dims = wb_dims(tbl_row + seq_len(nrow(cd)), 2),
                  numfmt = numfmt_for(cmet$unit))
    wb$set_col_widths("Chart", cols = 1:3, widths = c(30, 10, 7))

    # wb_data() points at the table's cells (header + rows, columns A:B).
    # Building the chart from it makes the chart refer to those cells, e.g.
    # Chart!$B$5:$B$152, instead of embedding a copy of the numbers. The
    # Year column (C) is shown for the reader but not plotted.
    dat <- wb_data(wb, "Chart", dims = wb_dims(tbl_row + 0:nrow(cd), 1:2))
    g <- ms_barchart(dat, x = geo_lbl, y = "Value")
    g <- chart_settings(g, dir = "vertical", gap_width = 40)   # upright bars, narrow gaps
    if (!is.null(ch$color)) {                                  # optional bar colour from config
      g <- chart_data_fill(g, values = c(Value = ch$color))
      g <- chart_data_stroke(g, values = c(Value = ch$color))
    }
    g <- chart_labels(g, title = ch$title, xlab = NULL,
                      ylab = ch$y_label %||% axis_label_for(cmet$unit))
    g <- chart_ax_x(g, rotation = -90)                         # country names run vertically
    g <- chart_ax_y(g, num_fmt = numfmt_for(cmet$unit))
    g <- apply_theme(g, legend = "n")                          # one series, no legend needed

    # Chart size: as wide as needed for every country name to fit. A rotated
    # 8pt label needs ~14 px, a default Excel column is ~64 px, plus 3 columns
    # for the y axis; never narrower than 15 columns. Placed from column E
    # (5) at row 4, 34 rows tall.
    width_cols <- max(15L, ceiling(nrow(cd) * 14 / 64) + 3L)
    wb$add_mschart("Chart", dims = paste0(wb_dims(tbl_row, 5), ":",
                                          wb_dims(tbl_row + 34, 4 + width_cols)),
                   graph = g)
    wb <- force_every_category_label(wb)   # stop Excel hiding every other name (see helper)
  } else {
    # --- line chart: one line per region over time ---
    # Chart table: one row per year, one column per region:
    #   Year | Central and Southern Asia | Sub-Saharan Africa | World
    # pivot_wider() turns the long data (region, year, value) into this shape.
    cd <- filter(d, indicator_id == ci) |>
      select(geo_name, year, value) |>
      pivot_wider(names_from = geo_name, values_from = value) |>
      arrange(year) |>
      rename(Year = year)
    regs <- setdiff(names(cd), "Year")   # the region columns = the lines
    tbl_row <- 4L
    wb$add_data("Chart", cd, dims = wb_dims(tbl_row, 1))
    style_header(wb, "Chart", wb_dims(tbl_row, seq_along(cd)))
    wb$add_numfmt("Chart", dims = wb_dims(tbl_row + seq_len(nrow(cd)), 1 + seq_along(regs)),
                  numfmt = numfmt_for(cmet$unit))
    wb$set_col_widths("Chart", cols = seq_along(cd), widths = c(7, rep(16, length(regs))))

    # As for the bar chart, the chart refers to the table's cells. Each
    # region column becomes one line (y = regs).
    dat <- wb_data(wb, "Chart", dims = wb_dims(tbl_row + 0:nrow(cd), seq_along(cd)))
    g <- ms_linechart(dat, x = "Year", y = regs)
    g <- chart_settings(g, style = "lineMarker")                # lines with point markers
    g <- chart_labels(g, title = ch$title, xlab = NULL,
                      ylab = ch$y_label %||% axis_label_for(cmet$unit))
    g <- chart_ax_x(g, num_fmt = "0")     # show years as 2010, not 2,010.00
    g <- chart_ax_y(g, num_fmt = numfmt_for(cmet$unit))
    g <- apply_theme(g, legend = "b")     # legend at the bottom names the lines
    # Place the chart one empty column to the right of the table: 10 columns
    # wide, 24 rows tall.
    c0 <- length(cd) + 2L
    wb$add_mschart("Chart", dims = paste0(wb_dims(tbl_row, c0), ":", wb_dims(tbl_row + 24, c0 + 9)),
                   graph = g)
  }

  # Printing: landscape. A line chart and its short table fit on one page.
  # A bar chart's table is ~150-200 rows long, so it is fitted to the page
  # WIDTH only (fit_to_height = 0 means "as many pages tall as needed").
  if (ch$type == "line") {
    wb$page_setup("Chart", orientation = "landscape", fit_to_width = 1, fit_to_height = 1)
  } else {
    wb$page_setup("Chart", orientation = "landscape", fit_to_width = 1, fit_to_height = 0)
  }

  # ======================= Flags sheet =====================================
  # One row per flag from step 02, with readable column names and the
  # indicator's full label added.
  fl_out <- fl |>
    left_join(select(meta, indicator_id, indicator_label), by = "indicator_id") |>
    transmute(Severity = severity, Check = check, Indicator = indicator_id,
              `Indicator label` = indicator_label, !!geo_lbl := geo_name,
              Code = geo_code, Year = year, Detail = detail)
  # No flags: write a one-line note instead of an empty sheet.
  if (nrow(fl_out) == 0L) fl_out <- tibble(Severity = "none", Detail = "No flags raised.")
  wb$add_data("Flags", fl_out)
  style_header(wb, "Flags", wb_dims(1, seq_along(fl_out)))
  if (ncol(fl_out) > 2L) {   # i.e. real flags, not the "No flags raised." note
    wb$set_col_widths("Flags", cols = seq_along(fl_out), widths = c(10, 20, 15, 45, 28, 9, 7, 40))
    # Colour the Severity cell of each row with that severity's colour,
    # matching the cell colours on the Data sheet. +1 skips the header row.
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
  # A two-column Field | Value table describing where the workbook came from,
  # followed by a colour legend. tribble() lets the table be written row by
  # row, as it will appear on the sheet.
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
  # Rows 2-14 are the 13 rows of `about` (row 1 is the header).
  wb$add_font("About", dims = "A2:A14", bold = TRUE)
  wb$add_cell_style("About", dims = paste0("B2:B", nrow(about) + 1), wrap_text = TRUE, vertical = "top")
  wb$add_cell_style("About", dims = paste0("A2:A", nrow(about) + 1), vertical = "top")
  wb$add_fill("About", dims = "A3:B3", color = argb(flag_colors[["warning"]]))   # highlight the STATUS row
  wb$set_col_widths("About", cols = 1:2, widths = c(22, 110))

  # Colour legend, one blank row below the table: a swatch with the
  # severity name in column A, its meaning in column B.
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

  # ---- save -----------------------------------------------------------------
  path <- file.path(out_dir, paste0(v, ".xlsx"))
  wb$save(path, overwrite = TRUE)
  message(sprintf("Wrote %s (%d data rows, %d chart-table rows, %d flags)",
                  path, nrow(body_out), nrow(cd), nrow(fl)))
}
