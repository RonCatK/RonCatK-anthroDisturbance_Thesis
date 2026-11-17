#!/usr/bin/env Rscript
# Thesis figure: total linear disturbance length from Morris screening runs.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

default_opts <- list(
  metrics = file.path(project_root, "outputs", "sensitivity", "results", "morris_run_metrics_long.csv"),
  out_dir = file.path(project_root, "outputs", "sensitivity", "figures"),
  help = FALSE
)

parse_cli_args <- function(args) {
  opts <- default_opts
  if (!length(args)) return(opts)

  for (arg in args) {
    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
    } else if (grepl("^--metrics=", arg, ignore.case = TRUE)) {
      opts$metrics <- sub("^--metrics=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--out-dir=", arg, ignore.case = TRUE)) {
      opts$out_dir <- sub("^--out-dir=", "", arg, ignore.case = TRUE)
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/sensitivity/plot_morris_linear_length_boxplot.R [options]\n",
    "  --metrics=PATH    Morris run metrics CSV (default outputs/sensitivity/results/morris_run_metrics_long.csv)\n",
    "  --out-dir=PATH    Output folder for figures (default outputs/sensitivity/figures)\n",
    "  --help            Show this message\n"
  ))
}

opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help)) {
  print_usage()
  quit(status = 0)
}

metrics_path <- opts$metrics
out_dir <- opts$out_dir

if (!file.exists(metrics_path)) stop("Metrics file not found: ", metrics_path, call. = FALSE)

raw <- read.csv(metrics_path, stringsAsFactors = FALSE)
required <- c("year", "metric_id", "value", "useClusterMethod")
missing <- setdiff(required, names(raw))
if (length(missing)) {
  stop("Metrics file is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
}

df <- raw %>%
  filter(.data$metric_id == "total_linear_length_km") %>%
  mutate(
    year = suppressWarnings(as.integer(.data$year)),
    value = suppressWarnings(as.numeric(.data$value)),
    useClusterMethod = dplyr::case_when(
      tolower(.data$useClusterMethod) %in% c("true", "t", "1") ~ TRUE,
      tolower(.data$useClusterMethod) %in% c("false", "f", "0") ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  filter(.data$year %in% c(2021L, 2031L)) %>%
  filter(!is.na(.data$useClusterMethod)) %>%
  filter(!is.na(.data$value))

if (!nrow(df)) stop("No total_linear_length_km rows found for years 2021/2031.", call. = FALSE)

df <- df %>%
  mutate(
    useClusterMethod = factor(.data$useClusterMethod, levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE")),
    x = paste0(.data$year, " | ", .data$useClusterMethod)
  )

x_levels <- c("2021 | FALSE", "2021 | TRUE", "2031 | FALSE", "2031 | TRUE")
df$x <- factor(df$x, levels = x_levels[x_levels %in% unique(df$x)])

counts <- df %>%
  group_by(.data$x) %>%
  summarize(n = n(), .groups = "drop")
n_min <- min(counts$n, na.rm = TRUE)
n_max <- max(counts$n, na.rm = TRUE)
n_label <- if (is.na(n_min) || is.na(n_max)) "NA" else if (n_min == n_max) as.character(n_min) else sprintf("%d-%d", n_min, n_max)

subtitle <- paste0(
  "Morris screening runs; boxplots show median + IQR (whiskers = 1.5×IQR). n = ", n_label, " runs per group."
)

p <- ggplot(df, aes(x = x, y = value, fill = useClusterMethod)) +
  geom_boxplot(width = 0.62, outlier.alpha = 0.25) +
  scale_fill_manual(values = c("FALSE" = "#e31a1c", "TRUE" = "#1f78b4")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total simulated linear disturbance length (km)",
    subtitle = subtitle,
    x = "Year | useClusterMethod",
    y = "Total linear disturbance length (km)",
    fill = "useClusterMethod"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_dir, "morris_total_linear_length_boxplot.png"), plot = p, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "morris_total_linear_length_boxplot.pdf"), plot = p, width = 9, height = 5.5, dpi = 300)

message("Wrote figure to: ", out_dir)
