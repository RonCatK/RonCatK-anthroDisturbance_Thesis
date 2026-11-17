#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(ggplot2)
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}

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

key_metrics <- tibble::tribble(
  ~metric_id, ~sector, ~year, ~friendly, ~unit,
  "total_interval_new_area_km2", NA_character_, 2031, "Total interval new area (2011-2031 increment)", "km^2",
  "sector_current_total_area_km2", "forestry_cutblocks", 2031, "Forestry cutblocks total area (2031)", "km^2",
  "sector_current_total_area_km2", "settlements_settlements", 2031, "Settlements total area (2031)", "km^2",
  "sector_current_total_length_km", "oilGas_seismicLines", 2031, "Oil & gas seismic line length (2031)", "km"
)

build_metric_slug <- function(metric_id, sector, year) {
  raw <- if (is.null(sector) || is.na(sector) || !nzchar(sector)) {
    metric_id
  } else {
    paste(metric_id, sector, sep = "__")
  }
  if (!is.null(year) && !is.na(year)) raw <- paste0(raw, "_yr", year)
  gsub("[^A-Za-z0-9_-]+", "_", raw)
}

resolve_results_dir <- function(path) {
  candidates <- c(path, file.path(project_root, path))
  hit <- candidates[dir.exists(candidates)][1]
  if (is.na(hit)) {
    stop("Results directory not found: ", path, call. = FALSE)
  }
  normalizePath(hit, winslash = "/", mustWork = TRUE)
}

read_required_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path, call. = FALSE)
  }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

match_sector <- function(df, sector_val) {
  if (!"sector" %in% names(df)) df$sector <- NA_character_
  if (is.null(sector_val) || is.na(sector_val) || !nzchar(sector_val)) {
    df %>% filter(is.na(.data$sector) | !nzchar(coalesce(.data$sector, "")))
  } else {
    df %>% filter(tolower(coalesce(.data$sector, "")) == tolower(sector_val))
  }
}

filter_metric_row <- function(df, metric_id_val, sector_val, year_val) {
  out <- df %>% filter(.data$metric_id == metric_id_val)
  out <- match_sector(out, sector_val)
  if ("year" %in% names(out)) out <- out %>% filter(.data$year == year_val)
  out
}

pretty_disturbance_words <- function(x) {
  x <- as.character(x)
  x <- if_else(is.na(x) | !nzchar(x), NA_character_, x)
  x <- stringr::str_replace_all(x, "([a-z])([A-Z])", "\\1 \\2")
  x <- stringr::str_replace_all(x, "_", " ")
  stringr::str_to_sentence(x)
}

pretty_sector_label <- function(sector) {
  sector <- as.character(sector)
  sector <- if_else(is.na(sector) | !nzchar(sector), NA_character_, sector)
  group <- stringr::str_extract(sector, "^[^_]+")
  klass <- stringr::str_replace(sector, "^[^_]+_", "")
  group_label <- dplyr::case_when(
    group == "oilGas" ~ "Oil & gas",
    group == "forestry" ~ "Forestry",
    group == "mining" ~ "Mining",
    group == "roads" ~ "Roads",
    group == "settlements" ~ "Settlements",
    TRUE ~ group
  )
  klass_label <- pretty_disturbance_words(klass)
  dplyr::case_when(
    is.na(sector) ~ NA_character_,
    is.na(group) ~ NA_character_,
    tolower(coalesce(klass, "")) == tolower(coalesce(group, "")) ~ group_label,
    TRUE ~ paste(group_label, klass_label)
  )
}

plot_sitesel_key_metrics <- function(sitesel_design, out_path) {
  df <- sitesel_design %>%
    inner_join(key_metrics, by = c("metric_id", "sector", "year")) %>%
    mutate(
      siteSelectionAsDistributing = if_else(
        is.na(siteSelectionAsDistributing) | !nzchar(siteSelectionAsDistributing),
        "unspecified",
        siteSelectionAsDistributing
      ),
      metric_label = sprintf("%s (%s)", friendly, unit),
      metric_label = factor(metric_label, levels = unique(metric_label))
    )
  if (!nrow(df)) {
    warning("No site-selection rows found for key metrics; skipping plot.", call. = FALSE)
    return(invisible(NULL))
  }
  p <- ggplot(df, aes(x = siteSelectionAsDistributing, y = value_median)) +
    geom_col(fill = "#2C7FB8", alpha = 0.85) +
    geom_errorbar(aes(ymin = value_p05, ymax = value_p95), width = 0.2, color = "#08306B") +
    facet_wrap(~metric_label, scales = "free_y", ncol = 2) +
    labs(
      title = "UA site-selection sensitivity (design medians)",
      x = "siteSelectionAsDistributing",
      y = "Median across replicates (error bars: p05–p95)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(out_path, plot = p, width = 12, height = 7, dpi = 160)
  message("Saved figure: ", out_path)
}

option_list <- list(
  optparse::make_option(
    c("--base-run"),
    type = "character",
    default = "ua_base",
    help = "Run name for base replicates (default: ua_base).",
    metavar = "RUN"
  ),
  optparse::make_option(
    c("--random-runs"),
    type = "character",
    default = paste0("ua_random_", sprintf("%03d", 1:8), collapse = ","),
    help = "Comma-separated random design run_name values (default: ua_random_001..008).",
    metavar = "RUNS"
  ),
  optparse::make_option(
    c("--sitesel-runs"),
    type = "character",
    default = paste0("ua_sitesel_", sprintf("%03d", 1:4), collapse = ","),
    help = "Comma-separated site-selection run_name values (default: ua_sitesel_001..004).",
    metavar = "RUNS"
  ),
  optparse::make_option(
    c("--results-dir"),
    type = "character",
    default = file.path("outputs", "uncertainty", "results"),
    help = "Results directory containing ua_*_summary CSVs (default: outputs/uncertainty/results).",
    metavar = "DIR"
  )
)

main <- function() {
  opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
  results_dir <- resolve_results_dir(opts$`results-dir`)
  figures_dir <- file.path(results_dir, "figures")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

  base_run <- opts$`base-run`
  random_runs <- parse_run_names(opts$`random-runs`)
  sitesel_runs <- parse_run_names(opts$`sitesel-runs`)
  if (!length(random_runs)) stop("--random-runs must include at least one run name.", call. = FALSE)
  if (!length(sitesel_runs)) stop("--sitesel-runs must include at least one run name.", call. = FALSE)

  random_label <- make_run_label(random_runs)
  sitesel_label <- make_run_label(sitesel_runs)

  base_rep_path <- file.path(results_dir, sprintf("ua_replicates_summary_%s.csv", base_run))
  random_global_path <- file.path(results_dir, sprintf("ua_global_summary_%s.csv", random_label))
  sitesel_global_path <- file.path(results_dir, sprintf("ua_global_summary_%s.csv", sitesel_label))
  sitesel_design_path <- file.path(results_dir, sprintf("ua_design_summary_%s.csv", sitesel_label))
  base_run_metrics_path <- file.path(results_dir, sprintf("ua_run_metrics_%s.csv", base_run))

  base_rep <- read_required_csv(base_rep_path)
  random_global <- read_required_csv(random_global_path)
  sitesel_global <- read_required_csv(sitesel_global_path)
  sitesel_design <- read_required_csv(sitesel_design_path)
  base_run_metrics <- if (file.exists(base_run_metrics_path)) {
    suppressMessages(readr::read_csv(base_run_metrics_path, show_col_types = FALSE))
  } else {
    tibble()
  }

  base_iqr <- tibble()
  if (nrow(base_run_metrics)) {
    base_iqr <- base_run_metrics %>%
      filter(.data$mode == "replicates", .data$run_name == base_run) %>%
      group_by(.data$metric_id, .data$sector, .data$year) %>%
      summarise(base_iqr = stats::IQR(.data$value, na.rm = TRUE), .groups = "drop")
  }

  combined <- key_metrics %>%
    mutate(
      base_n = NA_integer_,
      base_median = NA_real_,
      base_p05 = NA_real_,
      base_p95 = NA_real_,
      base_cv = NA_real_,
      base_iqr = NA_real_,
      random_n_designs = NA_integer_,
      random_median = NA_real_,
      random_p05 = NA_real_,
      random_p95 = NA_real_,
      random_shift_pct = NA_real_,
      sitesel_n_designs = NA_integer_,
      sitesel_median = NA_real_,
      sitesel_p05 = NA_real_,
      sitesel_p95 = NA_real_,
      sitesel_shift_pct = NA_real_
    )

  for (idx in seq_len(nrow(key_metrics))) {
    row <- key_metrics[idx, ]
    base_row <- filter_metric_row(base_rep, row$metric_id, row$sector, row$year)
    rand_row <- filter_metric_row(random_global, row$metric_id, row$sector, row$year)
    site_row <- filter_metric_row(sitesel_global, row$metric_id, row$sector, row$year)

    if (nrow(base_row) >= 1) {
      combined$base_n[idx] <- base_row$n_runs[[1]] %||% NA_integer_
      combined$base_median[idx] <- base_row$value_median[[1]] %||% NA_real_
      combined$base_p05[idx] <- base_row$value_p05[[1]] %||% NA_real_
      combined$base_p95[idx] <- base_row$value_p95[[1]] %||% NA_real_
      base_mean <- base_row$value_mean[[1]] %||% NA_real_
      base_sd <- base_row$value_sd[[1]] %||% NA_real_
      combined$base_cv[idx] <- if (!is.na(base_mean) && !is.na(base_sd) && base_mean != 0) {
        base_sd / base_mean
      } else {
        NA_real_
      }
    }
    if (nrow(rand_row) >= 1) {
      combined$random_n_designs[idx] <- rand_row$n_designs[[1]] %||% NA_integer_
      combined$random_median[idx] <- rand_row$median_of_medians[[1]] %||% NA_real_
      combined$random_p05[idx] <- rand_row$p05_median[[1]] %||% NA_real_
      combined$random_p95[idx] <- rand_row$p95_median[[1]] %||% NA_real_
    }
    if (nrow(site_row) >= 1) {
      combined$sitesel_n_designs[idx] <- site_row$n_designs[[1]] %||% NA_integer_
      combined$sitesel_median[idx] <- site_row$median_of_medians[[1]] %||% NA_real_
      combined$sitesel_p05[idx] <- site_row$p05_median[[1]] %||% NA_real_
      combined$sitesel_p95[idx] <- site_row$p95_median[[1]] %||% NA_real_
    }

    if (nrow(base_iqr)) {
      iqr_row <- filter_metric_row(base_iqr, row$metric_id, row$sector, row$year)
      if (nrow(iqr_row) >= 1) {
        combined$base_iqr[idx] <- iqr_row$base_iqr[[1]] %||% NA_real_
      }
    }
    base_med <- combined$base_median[idx]
    if (!is.na(base_med) && base_med != 0) {
      combined$random_shift_pct[idx] <- (combined$random_median[idx] - base_med) / base_med * 100
      combined$sitesel_shift_pct[idx] <- (combined$sitesel_median[idx] - base_med) / base_med * 100
    }
  }

  out_combined <- file.path(results_dir, "ua_key_metrics_summary.csv")
  readr::write_csv(combined, out_combined)
  message("Wrote key-metrics summary: ", out_combined)

  sitesel_key <- sitesel_design %>%
    inner_join(key_metrics, by = c("metric_id", "sector", "year")) %>%
    select(
      design_id,
      siteSelectionAsDistributing,
      metric_id,
      sector,
      year,
      friendly,
      unit,
      n_runs,
      value_median,
      value_p05,
      value_p95
    ) %>%
    arrange(metric_id, sector, year, siteSelectionAsDistributing)

  out_sitesel <- file.path(results_dir, "ua_sitesel_key_metrics.csv")
  readr::write_csv(sitesel_key, out_sitesel)
  message("Wrote site-selection key metrics: ", out_sitesel)

  sitesel_overview <- sitesel_design %>%
    filter(
      .data$year == 2031,
      .data$metric_id %in% c(
        "total_interval_new_area_km2",
        "sector_interval_new_area_km2",
        "sector_current_total_area_km2",
        "sector_current_total_length_km"
      )
    ) %>%
    mutate(
      siteSelectionAsDistributing = if_else(
        is.na(siteSelectionAsDistributing) | !nzchar(siteSelectionAsDistributing),
        "unspecified",
        siteSelectionAsDistributing
      ),
      sector_group = stringr::str_extract(.data$sector, "^[^_]+"),
      disturbance_class = stringr::str_replace(.data$sector, "^[^_]+_", ""),
      sector_label = pretty_sector_label(.data$sector),
      unit = case_when(
        .data$metric_id == "sector_current_total_length_km" ~ "km",
        TRUE ~ "km^2"
      ),
      friendly = case_when(
        .data$metric_id == "total_interval_new_area_km2" ~ "Total interval new area (2031)",
        .data$metric_id == "sector_interval_new_area_km2" ~ paste0(sector_label, " interval new area (", year, ")"),
        .data$metric_id == "sector_current_total_area_km2" ~ paste0(sector_label, " total area (", year, ")"),
        .data$metric_id == "sector_current_total_length_km" ~ paste0(sector_label, " total length (", year, ")"),
        TRUE ~ paste(.data$metric_id, .data$sector, .data$year)
      )
    ) %>%
    select(
      design_id,
      siteSelectionAsDistributing,
      totalDisturbanceRate,
      clusterDistance,
      useClusterMethod,
      metric_id,
      sector,
      sector_group,
      disturbance_class,
      year,
      friendly,
      unit,
      n_runs,
      value_median,
      value_p05,
      value_p95
    ) %>%
    arrange(.data$metric_id, .data$sector_group, .data$disturbance_class, .data$siteSelectionAsDistributing)

  out_overview <- file.path(results_dir, "ua_sitesel_benchmark_overview_2031.csv")
  readr::write_csv(sitesel_overview, out_overview)
  message("Wrote site-selection benchmark overview: ", out_overview)

  sitesel_ranges <- sitesel_overview %>%
    group_by(.data$metric_id, .data$sector, .data$sector_group, .data$disturbance_class, .data$year, .data$friendly, .data$unit) %>%
    summarise(
      n_designs = n_distinct(.data$design_id),
      n_runs_per_design = if_else(n_distinct(.data$n_runs) == 1, first(.data$n_runs), NA_integer_),
      min_median = min(.data$value_median, na.rm = TRUE),
      max_median = max(.data$value_median, na.rm = TRUE),
      range_median = max_median - min_median,
      min_p05 = min(.data$value_p05, na.rm = TRUE),
      max_p95 = max(.data$value_p95, na.rm = TRUE),
      range_p05_p95 = max_p95 - min_p05,
      .groups = "drop"
    )

  min_cfg <- sitesel_overview %>%
    group_by(.data$metric_id, .data$sector, .data$year) %>%
    slice_min(order_by = .data$value_median, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      metric_id = .data$metric_id,
      sector = .data$sector,
      year = .data$year,
      min_design_id = .data$design_id,
      min_siteSelectionAsDistributing = .data$siteSelectionAsDistributing,
      min_value_median = .data$value_median
    )

  max_cfg <- sitesel_overview %>%
    group_by(.data$metric_id, .data$sector, .data$year) %>%
    slice_max(order_by = .data$value_median, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      metric_id = .data$metric_id,
      sector = .data$sector,
      year = .data$year,
      max_design_id = .data$design_id,
      max_siteSelectionAsDistributing = .data$siteSelectionAsDistributing,
      max_value_median = .data$value_median
    )

  sitesel_ranges <- sitesel_ranges %>%
    left_join(min_cfg, by = c("metric_id", "sector", "year")) %>%
    left_join(max_cfg, by = c("metric_id", "sector", "year")) %>%
    arrange(desc(.data$range_median), .data$metric_id, .data$sector)

  out_ranges <- file.path(results_dir, "ua_sitesel_benchmark_ranges_2031.csv")
  readr::write_csv(sitesel_ranges, out_ranges)
  message("Wrote site-selection benchmark ranges: ", out_ranges)

  fig_sitesel <- file.path(figures_dir, "ua_sitesel_key_metrics_by_site.png")
  plot_sitesel_key_metrics(sitesel_design, fig_sitesel)
}

tryCatch(main(), error = function(e) {
  message("summarize_ua_results.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
