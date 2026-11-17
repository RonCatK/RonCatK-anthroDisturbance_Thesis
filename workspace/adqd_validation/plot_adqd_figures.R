#!/usr/bin/env Rscript
# Build ADQD thesis figures: summary metrics and grid corroboration maps.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(terra)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

summary_defaults <- list(
  metrics_dir = file.path(project_root, "docs", "traceability", "evidence", "metrics"),
  out_dir = file.path(project_root, "outputs", "adqd_metrics", "figures"),
  class_mode = "crosswalk"
)

grid_defaults <- list(
  simulation_root = file.path(project_root, "outputs", "adqd_validation", "ADQD_HOLDOUT"),
  bead_root = file.path(project_root, "data", "raw", "ECCC"),
  study_area = file.path(project_root, "data", "study_area", "NWT_boundary.shp"),
  baseline_year = 2010L,
  comparison_year = 2020L,
  replicates = 1:10,
  line_buffer = 30,
  polygon_buffer = 0,
  raster_resolution = 100,
  grid_km = 10,
  class_mode = "crosswalk",
  year_rule = "increment",
  no_year_filter = FALSE,
  all_scenarios = FALSE,
  write_panels = TRUE,
  write_triptych = TRUE,
  triptych_width = 13.0,
  triptych_height = 6.5,
  base_size = 11,
  out_dir = file.path(project_root, "outputs", "adqd_metrics", "figures"),
  out_prefix = "adqd_holdout_standard_grid10km"
)

default_opts <- list(
  mode = "all",
  summary = summary_defaults,
  grid = grid_defaults,
  help = FALSE
)

parse_rep_list <- function(raw) {
  vals <- unique(suppressWarnings(as.integer(trimws(unlist(strsplit(raw, "[,;]"))))))
  vals <- vals[!is.na(vals)]
  if (!length(vals)) integer(0) else vals
}

parse_cli_args <- function(args) {
  opts <- default_opts
  if (!length(args)) return(opts)

  for (arg in args) {
    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
    } else if (grepl("^--mode=", arg, ignore.case = TRUE)) {
      opts$mode <- tolower(sub("^--mode=", "", arg, ignore.case = TRUE))
    } else if (arg == "--summary-only") {
      opts$mode <- "summary"
    } else if (arg == "--grid-only") {
      opts$mode <- "grid"
    } else if (grepl("^--metrics-dir=", arg, ignore.case = TRUE)) {
      opts$summary$metrics_dir <- sub("^--metrics-dir=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--summary-class-mode=", arg, ignore.case = TRUE)) {
      opts$summary$class_mode <- tolower(sub("^--summary-class-mode=", "", arg, ignore.case = TRUE))
    } else if (grepl("^--grid-class-mode=", arg, ignore.case = TRUE)) {
      opts$grid$class_mode <- tolower(sub("^--grid-class-mode=", "", arg, ignore.case = TRUE))
    } else if (grepl("^--class-mode=", arg, ignore.case = TRUE)) {
      val <- tolower(sub("^--class-mode=", "", arg, ignore.case = TRUE))
      opts$summary$class_mode <- val
      if (val != "all") {
        opts$grid$class_mode <- val
      }
    } else if (grepl("^--out-dir=", arg, ignore.case = TRUE)) {
      val <- sub("^--out-dir=", "", arg, ignore.case = TRUE)
      opts$summary$out_dir <- val
      opts$grid$out_dir <- val
    } else if (grepl("^--summary-out-dir=", arg, ignore.case = TRUE)) {
      opts$summary$out_dir <- sub("^--summary-out-dir=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--grid-out-dir=", arg, ignore.case = TRUE)) {
      opts$grid$out_dir <- sub("^--grid-out-dir=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--simulation-root=", arg, ignore.case = TRUE)) {
      opts$grid$simulation_root <- sub("^--simulation-root=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--bead-root=", arg, ignore.case = TRUE)) {
      opts$grid$bead_root <- sub("^--bead-root=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--study-area=", arg, ignore.case = TRUE)) {
      opts$grid$study_area <- sub("^--study-area=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--baseline-year=", arg, ignore.case = TRUE)) {
      opts$grid$baseline_year <- suppressWarnings(as.integer(sub("^--baseline-year=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--comparison-year=", arg, ignore.case = TRUE)) {
      opts$grid$comparison_year <- suppressWarnings(as.integer(sub("^--comparison-year=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--replicates=", arg, ignore.case = TRUE)) {
      vals <- parse_rep_list(sub("^--replicates=", "", arg, ignore.case = TRUE))
      if (length(vals)) opts$grid$replicates <- vals
    } else if (grepl("^--line-buffer=", arg, ignore.case = TRUE)) {
      opts$grid$line_buffer <- suppressWarnings(as.numeric(sub("^--line-buffer=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--polygon-buffer=", arg, ignore.case = TRUE)) {
      opts$grid$polygon_buffer <- suppressWarnings(as.numeric(sub("^--polygon-buffer=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--resolution=", arg, ignore.case = TRUE)) {
      opts$grid$raster_resolution <- suppressWarnings(as.numeric(sub("^--resolution=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--grid-km=", arg, ignore.case = TRUE)) {
      opts$grid$grid_km <- suppressWarnings(as.numeric(sub("^--grid-km=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--year-rule=", arg, ignore.case = TRUE)) {
      opts$grid$year_rule <- tolower(trimws(sub("^--year-rule=", "", arg, ignore.case = TRUE)))
    } else if (identical(arg, "--no-year-filter")) {
      opts$grid$no_year_filter <- TRUE
    } else if (identical(arg, "--all-scenarios")) {
      opts$grid$all_scenarios <- TRUE
    } else if (identical(arg, "--triptych-only")) {
      opts$grid$write_panels <- FALSE
      opts$grid$write_triptych <- TRUE
    } else if (identical(arg, "--panels-only")) {
      opts$grid$write_panels <- TRUE
      opts$grid$write_triptych <- FALSE
    } else if (identical(arg, "--a4") || identical(arg, "--a4-landscape")) {
      opts$grid$triptych_width <- 11.69
      opts$grid$triptych_height <- 8.27
      opts$grid$base_size <- max(opts$grid$base_size, 12)
    } else if (grepl("^--triptych-width=", arg, ignore.case = TRUE)) {
      opts$grid$triptych_width <- suppressWarnings(as.numeric(sub("^--triptych-width=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--triptych-height=", arg, ignore.case = TRUE)) {
      opts$grid$triptych_height <- suppressWarnings(as.numeric(sub("^--triptych-height=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--base-size=", arg, ignore.case = TRUE)) {
      opts$grid$base_size <- suppressWarnings(as.numeric(sub("^--base-size=", "", arg, ignore.case = TRUE)))
    } else if (grepl("^--out-prefix=", arg, ignore.case = TRUE)) {
      opts$grid$out_prefix <- sub("^--out-prefix=", "", arg, ignore.case = TRUE)
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/adqd_validation/plot_adqd_figures.R [options]\n",
    "  --mode=MODE             all | summary | grid (default all)\n",
    "  --summary-only          Shortcut for --mode=summary\n",
    "  --grid-only             Shortcut for --mode=grid\n",
    "  --metrics-dir=PATH      Folder with adqd_*__adqd_summary.csv files (default docs/traceability/evidence/metrics)\n",
    "  --class-mode=MODE       Summary class mode (crosswalk, native, all). Also sets grid mode when not 'all'.\n",
    "  --summary-class-mode=MODE  Summary class mode override\n",
    "  --grid-class-mode=MODE     Grid class mode override (crosswalk, native)\n",
    "  --out-dir=PATH          Output folder for figures (default outputs/adqd_metrics/figures)\n",
    "\n",
    "Grid map options:\n",
    "  --all-scenarios         Generate maps for all 4 ADQD scenarios\n",
    "  --simulation-root=DIR   ADQD output root (default outputs/adqd_validation/ADQD_HOLDOUT)\n",
    "  --bead-root=DIR         BEAD archives root (default data/raw/ECCC)\n",
    "  --study-area=FILE       Study area polygon (default data/study_area/NWT_boundary.shp)\n",
    "  --baseline-year=YYYY    Baseline year (default 2010)\n",
    "  --comparison-year=YYYY  Comparison year (default 2020)\n",
    "  --replicates=LIST       Replicate IDs (default 1:10)\n",
    "  --line-buffer=M         Line buffer in metres (default 30)\n",
    "  --polygon-buffer=M      Polygon buffer in metres (default 0)\n",
    "  --resolution=M          Base raster resolution in metres (default 100)\n",
    "  --grid-km=K             Grid aggregation size in km (default 10)\n",
    "  --year-rule=RULE        increment or exact (default increment)\n",
    "  --no-year-filter        Disable simulated year filtering\n",
    "  --triptych-only         Write only the stitched (triptych) figure\n",
    "  --panels-only           Write only individual panel figures\n",
    "  --triptych-width=IN     Triptych width in inches\n",
    "  --triptych-height=IN    Triptych height in inches\n",
    "  --base-size=PT          Base font size in points\n",
    "  --out-prefix=NAME       Output file prefix (default adqd_holdout_standard_grid10km)\n",
    "  --help                  Show this message\n"
  ))
}

# ---- Summary figures (adqd_summary.csv) ----
read_adqd_summaries <- function(metrics_dir, class_mode) {
  files <- list.files(metrics_dir, pattern = "adqd_.*__adqd_summary\\.csv$", full.names = TRUE)
  if (!length(files)) {
    stop("No adqd_summary CSVs found in: ", metrics_dir, call. = FALSE)
  }

  dt <- bind_rows(lapply(files, function(path) {
    df <- read.csv(path, stringsAsFactors = FALSE)
    df$source_file <- basename(path)
    df
  }))

  required_cols <- c(
    "interval",
    "replicate",
    "class_mode",
    "analysis_mode",
    "prevalence_ref",
    "prevalence_sim",
    "grid_spearman_1km",
    "grid_spearman_5km",
    "grid_spearman_10km"
  )
  missing <- setdiff(required_cols, names(dt))
  if (length(missing)) {
    stop("Missing required columns in adqd_summary CSVs: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  target_modes <- c("VERIFICATION", "HOLDOUT", "VERIFICATION_CARIBOU", "HOLDOUT_CARIBOU")
  dt <- dt %>% filter(.data$analysis_mode %in% target_modes)
  if (!nrow(dt)) stop("No matching ADQD scenarios found in metrics files.", call. = FALSE)

  class_mode <- tolower(class_mode)
  if (!class_mode %in% c("crosswalk", "native", "all")) {
    stop("Invalid --class-mode. Use crosswalk, native, or all.", call. = FALSE)
  }
  if (class_mode != "all") {
    if (!class_mode %in% unique(tolower(dt$class_mode))) {
      stop("Class mode not present in metrics: ", class_mode, call. = FALSE)
    }
    dt <- dt %>% filter(tolower(.data$class_mode) == class_mode)
  }

  dt %>%
    mutate(
      replicate = suppressWarnings(as.integer(.data$replicate)),
      interval = as.character(.data$interval),
      period = case_when(
        .data$interval == "2010_2015" ~ "Verification (2010-2015)",
        .data$interval == "2010_2020" ~ "Hold-out (2010-2020)",
        TRUE ~ .data$interval
      ),
      footprint = if_else(grepl("CARIBOU", .data$analysis_mode), "Influence-zone 500 m buffer", "Standard footprint")
    )
}

summarize_replicates <- function(dt) {
  dt %>%
    group_by(.data$analysis_mode, .data$interval, .data$period, .data$footprint, .data$replicate) %>%
    summarize(
      prevalence_ref = mean(.data$prevalence_ref, na.rm = TRUE),
      prevalence_sim = mean(.data$prevalence_sim, na.rm = TRUE),
      grid_spearman_1km = mean(.data$grid_spearman_1km, na.rm = TRUE),
      grid_spearman_5km = mean(.data$grid_spearman_5km, na.rm = TRUE),
      grid_spearman_10km = mean(.data$grid_spearman_10km, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_mean_sd <- function(dt, value_col, group_cols) {
  dt %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      mean = mean(.data[[value_col]], na.rm = TRUE),
      sd = sd(.data[[value_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    ) %>%
    mutate(sd = if_else(is.na(.data$sd), 0, .data$sd))
}

run_summary_figures <- function(metrics_dir, out_dir, class_mode) {
  dt <- read_adqd_summaries(metrics_dir, class_mode)
  rep_df <- summarize_replicates(dt)

  period_levels <- c("Verification (2010-2015)", "Hold-out (2010-2020)")
  footprint_levels <- c("Standard footprint", "Influence-zone 500 m buffer")
  rep_df <- rep_df %>%
    mutate(
      period = factor(.data$period, levels = period_levels),
      footprint = factor(.data$footprint, levels = footprint_levels)
    )

  rep_counts <- rep_df %>%
    group_by(.data$footprint, .data$period) %>%
    summarize(n_reps = n_distinct(.data$replicate), .groups = "drop")
  n_min <- min(rep_counts$n_reps, na.rm = TRUE)
  n_max <- max(rep_counts$n_reps, na.rm = TRUE)
  n_label <- if (is.na(n_min) || is.na(n_max)) "NA" else if (n_min == n_max) as.character(n_min) else sprintf("%d-%d", n_min, n_max)

  subtitle_prevalence <- paste0(
    "Reference values from benchmark maps; simulated values are mean +/- 1 SD across replicates (n = ", n_label, ")."
  )
  subtitle_spearman <- paste0(
    "Mean +/- 1 SD across replicates (n = ", n_label, "). Higher rho implies stronger agreement."
  )

  prevalence_long <- rep_df %>%
    select(footprint, period, replicate, prevalence_ref, prevalence_sim) %>%
    pivot_longer(
      cols = c("prevalence_ref", "prevalence_sim"),
      names_to = "source",
      values_to = "value"
    ) %>%
    mutate(source = recode(.data$source, prevalence_ref = "Reference", prevalence_sim = "Simulated"))

  prevalence_summary <- summarize_mean_sd(prevalence_long, "value", c("footprint", "period", "source")) %>%
    mutate(source = factor(.data$source, levels = c("Reference", "Simulated")))

  prevalence_step <- 0.005
  prevalence_upper <- max(prevalence_summary$mean + prevalence_summary$sd, na.rm = TRUE)
  if (!is.finite(prevalence_upper)) prevalence_upper <- 0
  prevalence_upper <- ceiling(prevalence_upper / prevalence_step) * prevalence_step
  prevalence_breaks <- seq(0, prevalence_upper, by = prevalence_step)

  p_prevalence <- ggplot(prevalence_summary, aes(x = period, y = mean, color = source)) +
    geom_point(position = position_dodge(width = 0.35), size = 2.6) +
    geom_errorbar(
      aes(ymin = mean - sd, ymax = mean + sd),
      position = position_dodge(width = 0.35),
      width = 0.15
    ) +
    facet_wrap(~ footprint) +
    scale_color_manual(values = c(Reference = "#1b9e77", Simulated = "#d95f02")) +
    scale_y_continuous(
      breaks = prevalence_breaks,
      limits = c(0, prevalence_upper),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "Change prevalence in reference vs simulated disturbance maps",
      subtitle = subtitle_prevalence,
      x = "Period",
      y = "Proportion of pixels changed",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  grid_long <- rep_df %>%
    select(footprint, period, replicate, starts_with("grid_spearman_")) %>%
    pivot_longer(
      cols = starts_with("grid_spearman_"),
      names_to = "grid_km",
      values_to = "rho"
    ) %>%
    mutate(
      grid_km = as.numeric(gsub("grid_spearman_|km", "", .data$grid_km))
    )

  grid_summary <- summarize_mean_sd(grid_long, "rho", c("footprint", "period", "grid_km")) %>%
    mutate(
      curve = case_when(
        .data$footprint == "Standard footprint" & .data$period == "Verification (2010-2015)" ~ "Standard - Verification",
        .data$footprint == "Standard footprint" & .data$period == "Hold-out (2010-2020)" ~ "Standard - Hold-out",
        .data$footprint == "Influence-zone 500 m buffer" & .data$period == "Verification (2010-2015)" ~ "Influence-zone 500 m - Verification",
        .data$footprint == "Influence-zone 500 m buffer" & .data$period == "Hold-out (2010-2020)" ~ "Influence-zone 500 m - Hold-out",
        TRUE ~ paste0(.data$footprint, " - ", .data$period)
      ),
      curve = factor(
        curve,
        levels = c(
          "Standard - Verification",
          "Standard - Hold-out",
          "Influence-zone 500 m - Verification",
          "Influence-zone 500 m - Hold-out"
        )
      )
    )

  p_spearman <- ggplot(grid_summary, aes(x = grid_km, y = mean, color = curve, group = curve)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.4) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    scale_x_continuous(breaks = c(1, 5, 10)) +
    scale_color_manual(
      values = c(
        "Standard - Verification" = "#1f78b4",
        "Standard - Hold-out" = "#e31a1c",
        "Influence-zone 500 m - Verification" = "#33a02c",
        "Influence-zone 500 m - Hold-out" = "#ff7f00"
      )
    ) +
    labs(
      title = "Scale-dependent corroboration (Spearman rho)",
      subtitle = subtitle_spearman,
      x = "Aggregation grid size (km)",
      y = "Spearman rho",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 9)
    )

  p_spearman <- p_spearman + guides(color = guide_legend(nrow = 2, byrow = TRUE))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  save_plot <- function(plot, filename, width, height) {
    ggsave(file.path(out_dir, paste0(filename, ".png")), plot = plot, width = width, height = height, dpi = 300)
    ggsave(file.path(out_dir, paste0(filename, ".pdf")), plot = plot, width = width, height = height, dpi = 300)
  }

  save_plot(p_prevalence, "adqd_change_prevalence", 9, 5.5)
  save_plot(p_spearman, "adqd_grid_spearman", 9, 5.5)

  message("Summary figures written to: ", out_dir)
}

# ---- Grid corroboration maps ----
load_compute_map_metrics_env <- function(project_root) {
  src_path <- file.path(project_root, "workspace", "adqd_validation", "compute_map_metrics.R")
  if (!file.exists(src_path)) stop("Missing compute_map_metrics.R: ", src_path, call. = FALSE)

  old_opt <- getOption("adqd_compute_map_metrics.run_main")
  options(adqd_compute_map_metrics.run_main = FALSE)
  on.exit({
    if (is.null(old_opt)) {
      options(adqd_compute_map_metrics.run_main = NULL)
    } else {
      options(adqd_compute_map_metrics.run_main = old_opt)
    }
  }, add = TRUE)

  env <- new.env(parent = globalenv())
  sys.source(src_path, envir = env)
  env
}

scenario_presets <- function(project_root, grid_km) {
  fmt_prefix <- function(stem) sprintf("%s_grid%dkm", stem, as.integer(round(grid_km)))
  list(
    verification_standard = list(
      simulation_root = file.path(project_root, "outputs", "adqd_validation", "ADQD_VERIFICATION"),
      baseline_year = 2010L,
      comparison_year = 2015L,
      line_buffer = 30,
      polygon_buffer = 0,
      out_prefix = fmt_prefix("adqd_verification_standard")
    ),
    holdout_standard = list(
      simulation_root = file.path(project_root, "outputs", "adqd_validation", "ADQD_HOLDOUT"),
      baseline_year = 2010L,
      comparison_year = 2020L,
      line_buffer = 30,
      polygon_buffer = 0,
      out_prefix = fmt_prefix("adqd_holdout_standard")
    ),
    verification_caribou500m = list(
      simulation_root = file.path(project_root, "outputs", "adqd_validation", "ADQD_VERIFICATION_CARIBOU"),
      baseline_year = 2010L,
      comparison_year = 2015L,
      line_buffer = 500,
      polygon_buffer = 500,
      out_prefix = fmt_prefix("adqd_verification_caribou500m")
    ),
    holdout_caribou500m = list(
      simulation_root = file.path(project_root, "outputs", "adqd_validation", "ADQD_HOLDOUT_CARIBOU"),
      baseline_year = 2010L,
      comparison_year = 2020L,
      line_buffer = 500,
      polygon_buffer = 500,
      out_prefix = fmt_prefix("adqd_holdout_caribou500m")
    )
  )
}

make_map_df <- function(r, value_name) {
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  if (ncol(df) < 3) stop("Unexpected raster->data.frame conversion.", call. = FALSE)
  names(df)[3] <- value_name
  df
}

theme_map <- function(base_size) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.margin = margin(1, 1, 1, 1, unit = "mm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

extract_legend_grob <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- gtable::gtable_filter(g, "guide-box")
  if (inherits(legend, "gtable")) {
    return(legend)
  }
  NULL
}

assemble_triptych_grob <- function(p_ref, p_sim, p_diff, title, subtitle, base_size) {
  legend_seq <- extract_legend_grob(
    p_sim + theme(legend.position = "bottom", legend.direction = "horizontal")
  )
  legend_div <- extract_legend_grob(
    p_diff + theme(legend.position = "bottom", legend.direction = "horizontal")
  )

  p_ref_noleg <- p_ref + theme(legend.position = "none")
  p_sim_noleg <- p_sim + theme(legend.position = "none")
  p_diff_noleg <- p_diff + theme(legend.position = "none")

  g_ref <- ggplotGrob(p_ref_noleg)
  g_sim <- ggplotGrob(p_sim_noleg)
  g_diff <- ggplotGrob(p_diff_noleg)

  title_grob <- grid::textGrob(
    title,
    x = 0, hjust = 0,
    gp = grid::gpar(fontface = "bold", fontsize = base_size + 4, col = "grey10")
  )
  subtitle_grob <- grid::textGrob(
    subtitle,
    x = 0, hjust = 0,
    gp = grid::gpar(fontsize = base_size, col = "grey30")
  )

  widths <- grid::unit(c(1, 1, 1), "null")
  heights <- grid::unit.c(
    grid::unit(0.4, "in"),
    grid::unit(0.3, "in"),
    grid::unit(1, "null"),
    grid::unit(0.4, "in"),
    grid::unit(0.4, "in")
  )
  gt <- gtable::gtable(widths = widths, heights = heights)
  gt <- gtable::gtable_add_grob(gt, title_grob, t = 1, l = 1, r = 3)
  gt <- gtable::gtable_add_grob(gt, subtitle_grob, t = 2, l = 1, r = 3)
  gt <- gtable::gtable_add_grob(gt, g_ref, t = 3, l = 1)
  gt <- gtable::gtable_add_grob(gt, g_sim, t = 3, l = 2)
  gt <- gtable::gtable_add_grob(gt, g_diff, t = 3, l = 3)
  if (!is.null(legend_seq)) {
    gt <- gtable::gtable_add_grob(gt, legend_seq, t = 4, l = 1, r = 3)
  }
  if (!is.null(legend_div)) {
    gt <- gtable::gtable_add_grob(gt, legend_div, t = 5, l = 1, r = 3)
  }
  gt
}

run_grid_maps <- function(opts, env) {
  if (!opts$class_mode %in% c("crosswalk", "native")) {
    stop("Grid class mode must be crosswalk or native.", call. = FALSE)
  }

  scenario_list <- if (isTRUE(opts$all_scenarios)) {
    scenario_presets(project_root, opts$grid_km)
  } else {
    list(single = list())
  }

  for (scenario_name in names(scenario_list)) {
    scenario <- scenario_list[[scenario_name]]
    grid_opts <- opts
    for (nm in names(scenario)) {
      grid_opts[[nm]] <- scenario[[nm]]
    }

    simulation_root <- grid_opts$simulation_root
    bead_root <- grid_opts$bead_root
    study_area <- grid_opts$study_area
    baseline_year <- grid_opts$baseline_year
    comparison_year <- grid_opts$comparison_year
    replicates <- grid_opts$replicates
    line_buffer <- grid_opts$line_buffer
    polygon_buffer <- grid_opts$polygon_buffer
    raster_resolution <- grid_opts$raster_resolution
    grid_km <- grid_opts$grid_km
    class_mode <- grid_opts$class_mode
    year_rule <- grid_opts$year_rule
    no_year_filter <- grid_opts$no_year_filter
    out_dir <- grid_opts$out_dir
    out_prefix <- grid_opts$out_prefix
    write_panels <- grid_opts$write_panels
    write_triptych <- grid_opts$write_triptych
    triptych_width <- grid_opts$triptych_width
    triptych_height <- grid_opts$triptych_height
    base_size <- grid_opts$base_size

    if (!dir.exists(simulation_root)) {
      warning("Missing simulation outputs: ", simulation_root, immediate. = TRUE)
      next
    }

    study_area_sv <- env$ensure_spatvector(study_area)
    observed <- env$load_observed_interval(
      baseline_year = baseline_year,
      comparison_year = comparison_year,
      study_area = study_area_sv,
      bead_root = bead_root,
      line_buffer = line_buffer,
      polygon_buffer = polygon_buffer
    )

    sim_files_all <- env$parse_simulated_file_metadata(simulation_root)
    if (!nrow(sim_files_all)) {
      warning("No disturbance shapefiles found under ", simulation_root, immediate. = TRUE)
      next
    }

    class_mapper <- function(x) x
    crosswalk <- NULL
    if (class_mode == "crosswalk") {
      crosswalk <- env$build_evaluation_crosswalk()
      class_mapper <- function(x) env$map_class_to_crosswalk(x, crosswalk)
    }

    sim_index <- env$build_sim_file_index(
      file_dt = sim_files_all,
      baseline_year = baseline_year,
      comparison_year = comparison_year,
      year_rule = year_rule,
      no_year_filter = no_year_filter,
      replicates = replicates,
      class_mode = class_mode,
      class_filter = NULL,
      class_mapper = class_mapper
    )
    sim_selected <- sim_index[sim_index$selected == TRUE, , drop = FALSE]
    if (!nrow(sim_selected)) {
      warning("No simulated files selected for ", simulation_root, immediate. = TRUE)
      next
    }

    sim <- env$prepare_simulated_geoms(sim_selected, study_area_sv, line_buffer, polygon_buffer)
    sim_classes <- sim$geoms

    obs_classes <- observed$classes
    if (class_mode == "crosswalk" && !is.null(crosswalk)) {
      obs_classes <- env$apply_crosswalk_to_class_list(obs_classes, crosswalk)
      sim_classes <- lapply(sim_classes, env$apply_crosswalk_to_class_list, crosswalk = crosswalk)
    }

    template <- env$make_template_raster(study_area_sv, raster_resolution)
    base_res <- terra::res(template)[1]
    grid_factor <- max(1L, round((grid_km * 1000) / base_res))

    obs_bin <- env$make_binary_raster(obs_classes, template)
    obs_grid <- terra::aggregate(obs_bin, fact = grid_factor, fun = mean, na.rm = TRUE)

    sim_grid_list <- list()
    for (rep_id in replicates) {
      rep_key <- as.character(rep_id)
      class_list <- sim_classes[[rep_key]]
      if (is.null(class_list) || !length(class_list)) {
        warning("Missing simulated classes for replicate ", rep_id, " in ", simulation_root, immediate. = FALSE)
        next
      }
      sim_bin <- env$make_binary_raster(class_list, template)
      sim_grid_list[[length(sim_grid_list) + 1L]] <- terra::aggregate(sim_bin, fact = grid_factor, fun = mean, na.rm = TRUE)
    }
    if (!length(sim_grid_list)) {
      warning("No simulated grids produced for ", simulation_root, immediate. = TRUE)
      next
    }

    sim_mean <- if (length(sim_grid_list) == 1) {
      sim_grid_list[[1]]
    } else {
      terra::app(terra::rast(sim_grid_list), fun = mean, na.rm = TRUE)
    }
    diff_grid <- sim_mean - obs_grid

    study_area_sf <- sf::st_as_sf(study_area_sv)

    max_val <- max(
      terra::values(obs_grid, mat = FALSE),
      terra::values(sim_mean, mat = FALSE),
      na.rm = TRUE
    )
    if (!is.finite(max_val) || max_val <= 0) max_val <- 0.001

    diff_vals <- terra::values(diff_grid, mat = FALSE)
    diff_max <- max(abs(diff_vals), na.rm = TRUE)
    if (!is.finite(diff_max) || diff_max <= 0) diff_max <- 0.001

    seq_palette <- grDevices::colorRampPalette(c("#f7fbff", "#6baed6", "#08306b"))

    ref_df <- make_map_df(obs_grid, "value")
    sim_df <- make_map_df(sim_mean, "value")
    diff_df <- make_map_df(diff_grid, "value")

    base_theme <- theme_map(base_size)

    p_ref <- ggplot(ref_df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      geom_sf(data = study_area_sf, fill = NA, color = "grey20", linewidth = 0.2) +
      scale_fill_gradientn(
        colours = seq_palette(10),
        limits = c(0, max_val),
        name = "Disturbed fraction"
      ) +
      labs(title = "Reference disturbance fraction") +
      coord_sf(crs = sf::st_crs(study_area_sf), datum = NA) +
      base_theme

    p_sim <- ggplot(sim_df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      geom_sf(data = study_area_sf, fill = NA, color = "grey20", linewidth = 0.2) +
      scale_fill_gradientn(
        colours = seq_palette(10),
        limits = c(0, max_val),
        name = "Disturbed fraction"
      ) +
      labs(title = "Simulated disturbance fraction") +
      coord_sf(crs = sf::st_crs(study_area_sf), datum = NA) +
      base_theme

    p_diff <- ggplot(diff_df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      geom_sf(data = study_area_sf, fill = NA, color = "grey20", linewidth = 0.2) +
      scale_fill_gradient2(
        low = "#2166ac",
        mid = "#f7f7f7",
        high = "#b2182b",
        midpoint = 0,
        limits = c(-diff_max, diff_max),
        name = "Sim - Ref"
      ) +
      labs(title = "Difference (sim - ref)") +
      coord_sf(crs = sf::st_crs(study_area_sf), datum = NA) +
      base_theme

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    if (isTRUE(write_panels)) {
      ggsave(file.path(out_dir, paste0(out_prefix, "_ref.png")), plot = p_ref, width = 5.5, height = 4.5, dpi = 300)
      ggsave(file.path(out_dir, paste0(out_prefix, "_sim.png")), plot = p_sim, width = 5.5, height = 4.5, dpi = 300)
      ggsave(file.path(out_dir, paste0(out_prefix, "_diff.png")), plot = p_diff, width = 5.5, height = 4.5, dpi = 300)
      ggsave(file.path(out_dir, paste0(out_prefix, "_ref.pdf")), plot = p_ref, width = 5.5, height = 4.5)
      ggsave(file.path(out_dir, paste0(out_prefix, "_sim.pdf")), plot = p_sim, width = 5.5, height = 4.5)
      ggsave(file.path(out_dir, paste0(out_prefix, "_diff.pdf")), plot = p_diff, width = 5.5, height = 4.5)
    }

    if (isTRUE(write_triptych)) {
      title <- sprintf("ADQD grid corroboration (%s)", gsub("_", " ", out_prefix))
      subtitle <- sprintf("Baseline %d to %d, grid %dkm, class_mode=%s", baseline_year, comparison_year, as.integer(round(grid_km)), class_mode)
      triptych <- assemble_triptych_grob(p_ref, p_sim, p_diff, title, subtitle, base_size)
      ggsave(file.path(out_dir, paste0(out_prefix, "_triptych.png")), plot = triptych, width = triptych_width, height = triptych_height, dpi = 300)
      ggsave(file.path(out_dir, paste0(out_prefix, "_triptych.pdf")), plot = triptych, width = triptych_width, height = triptych_height)
    }

    message("Grid maps written to: ", out_dir, " (", out_prefix, ")")
  }
}

opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help)) {
  print_usage()
  quit(status = 0)
}

if (!opts$mode %in% c("summary", "grid", "all")) {
  stop("Invalid --mode. Use summary, grid, or all.", call. = FALSE)
}

run_summary <- opts$mode %in% c("summary", "all")
run_grid <- opts$mode %in% c("grid", "all")

if (run_grid && opts$grid$class_mode == "all") {
  stop("Grid mode does not support class_mode=all. Use crosswalk or native.", call. = FALSE)
}

if (run_summary) {
  run_summary_figures(opts$summary$metrics_dir, opts$summary$out_dir, opts$summary$class_mode)
}

if (run_grid) {
  env <- load_compute_map_metrics_env(project_root)
  run_grid_maps(opts$grid, env)
}
