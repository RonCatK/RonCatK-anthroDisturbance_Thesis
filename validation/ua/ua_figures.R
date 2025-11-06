suppressPackageStartupMessages({
  requireNamespace("data.table")
  requireNamespace("ggplot2")
  requireNamespace("glue")
})

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

pth <- file.path("results", "metrics_summary.csv")
stopifnot(file.exists(pth))
dt <- data.table::fread(pth)
if (!nrow(dt)) quit(save = "no")

plot_metric <- function(dt, metric) {
  d <- dt[metric == !!metric]
  if (!nrow(d)) return(invisible(NULL))
  # Ensure ordering
  d <- d[order(scenario_id, year)]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = year, y = mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "steelblue") +
    ggplot2::geom_line(color = "steelblue4", linewidth = 0.7) +
    ggplot2::facet_wrap(~ label, scales = "free_y") +
    ggplot2::labs(x = "Year", y = metric, title = glue::glue("Uncertainty bands: {metric}")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  outfile <- file.path("figures", glue::glue("uq_{metric}.png"))
  ggplot2::ggsave(outfile, p, width = 10, height = 6, dpi = 150)
  invisible(outfile)
}

# Primary metrics
f1 <- plot_metric(dt, "total_yearly_new_area_km2")
f2 <- plot_metric(dt, "sector_yearly_new_area_km2")

# Acceptance check (guarded)
if (Sys.getenv("UA_RUN_ACCEPTANCE", unset = "0") == "1") {
  stopifnot(file.exists(file.path("figures", "uq_total_yearly_new_area_km2.png")))
}

invisible(list(files = c(f1, f2)))

