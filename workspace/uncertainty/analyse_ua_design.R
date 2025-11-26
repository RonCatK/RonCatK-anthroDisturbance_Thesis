#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(tibble)
  library(grid)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

parse_run_names <- function(value) {
  if (is.null(value) || !nzchar(value)) return(character())
  vals <- unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE)
  trimmed <- trimws(vals)
  trimmed[nzchar(trimmed)]
}

make_run_label <- function(run_names) {
  if (!length(run_names)) return("ua_runs")
  if (length(run_names) == 1L) return(run_names[[1]])
  sanitized <- gsub("[^A-Za-z0-9_-]+", "_", run_names)
  paste0("batch_", paste0(sanitized, collapse = "_"))
}

option_list <- list(
  optparse::make_option(
    c("--run-name"),
    type = "character",
    default = NA_character_,
    help = "Comma-separated run_name values that identify the UA design batch.",
    metavar = "RUNS"
  ),
  optparse::make_option(
    c("--suite"),
    type = "character",
    default = "uncertainty",
    help = "Suite label (default: uncertainty).",
    metavar = "SUITE"
  )
)

load_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path, call. = FALSE)
  }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

is_blank_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L) return(TRUE)
  val <- x[[1]]
  if (is.na(val)) return(TRUE)
  val_chr <- as.character(val)
  !nzchar(val_chr)
}

key_metrics <- tibble::tribble(
  ~metric_id, ~sector, ~year, ~friendly, ~y_label,
  "total_yearly_new_area_km2", NA_character_, 2031, "Total yearly new area (cum 2011-2041 proxy)", "Area (km^2)",
  "sector_current_total_area_km2", "forestry_cutblocks", 2031, "Forestry cutblocks total area (2041)", "Area (km^2)",
  "sector_current_total_area_km2", "settlements_settlements", 2031, "Settlements total area (2041)", "Area (km^2)",
  "sector_current_total_length_km", "oilGas_seismicLines", 2031, "Oil & gas seismic line length (2041)", "Length (km)"
)

build_metric_slug <- function(metric_id, sector, year) {
  raw <- if (is.null(sector) || is.na(sector) || !nzchar(sector)) {
    metric_id
  } else {
    paste(metric_id, sector, sep = "__")
  }
  if (!is_blank_scalar(year)) {
    raw <- paste0(raw, "_yr", year)
  }
  gsub("[^A-Za-z0-9_-]+", "_", raw)
}

ensure_fig_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

format_range_message <- function(metric_label, stats_tbl) {
  sprintf(
    "%s – design medians range: min=%.3f, median=%.3f, max=%.3f (n=%d)",
    metric_label,
    stats_tbl$min,
    stats_tbl$median,
    stats_tbl$max,
    stats_tbl$n_obs
  )
}

plot_metric <- function(df, info, fig_dir, run_label) {
  metric_slug <- build_metric_slug(info$metric_id, info$sector, info$year)
  y_lab <- info$y_label %||% "Value"
  year_fragment <- if (!is_blank_scalar(info$year)) sprintf(" (year %s)", info$year) else ""
  plot_title <- sprintf("%s – %s%s", info$friendly, run_label, year_fragment)
  df <- df %>%
    mutate(
      siteSelectionAsDistributing = if_else(
        is.na(siteSelectionAsDistributing) | !nzchar(siteSelectionAsDistributing),
        "unspecified",
        siteSelectionAsDistributing
      ),
      clusterDistance = suppressWarnings(as.numeric(clusterDistance)),
      totalDisturbanceRate = suppressWarnings(as.numeric(totalDisturbanceRate))
    )
  primary_plot <- ggplot(df, aes(
    x = totalDisturbanceRate,
    y = value_median,
    color = clusterDistance,
    shape = siteSelectionAsDistributing
  )) +
    geom_point(size = 3, alpha = 0.9) +
    labs(
      title = plot_title,
      subtitle = "Median metric vs totalDisturbanceRate",
      x = "totalDisturbanceRate (%/year)",
      y = y_lab,
      color = "clusterDistance",
      shape = "siteSelection"
    ) +
    theme_minimal()

  secondary_available <- any(!is.na(df$clusterDistance)) && length(unique(df$clusterDistance[!is.na(df$clusterDistance)])) > 1
  secondary_plot <- ggplot(df, aes(
    x = clusterDistance,
    y = value_median,
    color = totalDisturbanceRate,
    shape = siteSelectionAsDistributing
  )) +
    geom_point(size = 3, alpha = 0.9) +
    labs(
      subtitle = "Median metric vs clusterDistance",
      x = "clusterDistance (m)",
      y = y_lab,
      color = "totalDisturbanceRate",
      shape = "siteSelection"
    ) +
    theme_minimal()

  n_rows <- if (secondary_available) 2 else 1
  fig_path <- file.path(fig_dir, sprintf("ua_design_%s_%s.png", run_label, metric_slug))
  grDevices::png(fig_path, width = 1400, height = 800, res = 130)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(n_rows, 1)))
  print(primary_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  if (secondary_available) {
    print(secondary_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  }
  message("Saved figure: ", fig_path)
}

analyse_metric <- function(design_df, info, fig_dir, run_label) {
  metric_filter <- design_df %>%
    filter(.data$metric_id == info$metric_id)
  if (!is_blank_scalar(info$sector)) {
    metric_filter <- metric_filter %>%
      filter(tolower(coalesce(.data$sector, "")) == tolower(info$sector))
  }
  if (!is_blank_scalar(info$year)) {
    metric_filter <- metric_filter %>% filter(.data$year == as.numeric(info$year))
  }

  if (!nrow(metric_filter)) {
    message("No rows for metric ", info$metric_id, " sector ", info$sector %||% "<NA>", "; skipping.")
    return(invisible(NULL))
  }

  metric_filter <- metric_filter %>%
    mutate(
      totalDisturbanceRate = suppressWarnings(as.numeric(totalDisturbanceRate)),
      clusterDistance = suppressWarnings(as.numeric(clusterDistance))
    )

  stats_tbl <- metric_filter %>%
    summarise(
      min = min(value_median, na.rm = TRUE),
      median = median(value_median, na.rm = TRUE),
      max = max(value_median, na.rm = TRUE),
      n_obs = n()
    )
  message(format_range_message(info$friendly, stats_tbl))

  lm_data <- metric_filter %>%
    select(value_median, totalDisturbanceRate, clusterDistance) %>%
    drop_na()
  if (nrow(lm_data) >= 3) {
    message("Linear model summary for ", info$friendly, ":")
    print(summary(lm(value_median ~ totalDisturbanceRate + clusterDistance, data = lm_data)))
  } else {
    message("Not enough rows with complete predictors for ", info$friendly, "; skipping linear model.")
  }

  plot_metric(metric_filter, info, fig_dir, run_label)
}

main <- function() {
  opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
  run_names <- parse_run_names(opts[["run-name"]])
  if (!length(run_names)) {
    stop("Argument --run-name is required (comma-separated allowed).", call. = FALSE)
  }
  suite <- opts$suite %||% "uncertainty"
  run_label <- make_run_label(run_names)
  message(sprintf("Analysing UA design summaries for suite=%s, runs=[%s], label=%s.", suite, paste(run_names, collapse = ","), run_label))

  results_dir <- file.path(project_root, "workspace", "uncertainty", "results")
  design_csv <- file.path(results_dir, sprintf("ua_design_summary_%s.csv", run_label))
  global_csv <- file.path(results_dir, sprintf("ua_global_summary_%s.csv", run_label))

  design_summary <- load_csv(design_csv)
  global_summary <- load_csv(global_csv)
  message(sprintf("Loaded design summary (%d rows) and global summary (%d rows).", nrow(design_summary), nrow(global_summary)))

  figures_dir <- ensure_fig_dir(file.path(results_dir, "figures"))

  for (idx in seq_len(nrow(key_metrics))) {
    row <- key_metrics[idx, ]
    info <- list(
      metric_id = row$metric_id,
      sector = row$sector,
      year = row$year,
      friendly = row$friendly,
      y_label = row$y_label
    )
    analyse_metric(design_summary, info, figures_dir, run_label)
  }

  message("UA design analysis complete.")
}

tryCatch(main(), error = function(e) {
  message("analyse_ua_design.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
