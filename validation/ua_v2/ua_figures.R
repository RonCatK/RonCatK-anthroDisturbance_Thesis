#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(glue)
})

parse_cli_args <- function(args) {
  opts <- list(
    metrics_file = file.path("results", "metrics_summary.csv"),
    output_dir = "figures",
    help = FALSE
  )
  if (!length(args)) return(opts)
  for (arg in args) {
    if (identical(arg, "--help") || identical(arg, "-h")) {
      opts$help <- TRUE
    } else if (grepl("^--metrics=", arg, ignore.case = TRUE)) {
      opts$metrics_file <- sub("^--metrics=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--output-dir=", arg, ignore.case = TRUE)) {
      opts$output_dir <- sub("^--output-dir=", "", arg, ignore.case = TRUE)
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript validation/ua_v2/ua_figures.R [options]\n",
    "  --metrics=PATH     Path to metrics_summary.csv (default results/metrics_summary.csv)\n",
    "  --output-dir=DIR   Directory to write figures (default ./figures)\n",
    "  --help             Show this message\n"
  ))
}

plot_metric <- function(dt, metric_name, out_dir) {
  d <- dt[metric == metric_name]
  if (!nrow(d)) return(invisible(NULL))
  d <- d[order(scenario_id, year)]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = year, y = mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "steelblue") +
    ggplot2::geom_line(color = "steelblue4", linewidth = 0.7) +
    ggplot2::facet_wrap(~ label, scales = "free_y") +
    ggplot2::labs(
      x = "Year",
      y = metric_name,
      title = glue::glue("Uncertainty bands: {metric_name}")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  outfile <- file.path(out_dir, glue::glue("uq_{metric_name}.png"))
  ggplot2::ggsave(outfile, p, width = 10, height = 6, dpi = 150)
  invisible(outfile)
}

main <- function() {
  opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    print_usage()
    quit(save = "no", status = 0, runLast = FALSE)
  }
  metrics_path <- normalizePath(opts$metrics_file, winslash = "/", mustWork = FALSE)
  if (!file.exists(metrics_path)) {
    message(sprintf("Metrics summary not found at %s; skipping figures.", metrics_path))
    quit(save = "no", status = 0, runLast = FALSE)
  }
  dt <- data.table::fread(metrics_path)
  if (!nrow(dt)) {
    message("Metrics summary is empty; no figures generated.")
    quit(save = "no", status = 0, runLast = FALSE)
  }
  out_dir <- normalizePath(opts$output_dir, winslash = "/", mustWork = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list(
    plot_metric(dt, "total_yearly_new_area_km2", out_dir),
    plot_metric(dt, "sector_yearly_new_area_km2", out_dir)
  )
  if (Sys.getenv("UA_RUN_ACCEPTANCE", unset = "0") == "1") {
    stopifnot(file.exists(file.path(out_dir, "uq_total_yearly_new_area_km2.png")))
  }
  invisible(files)
}

main()
