#!/usr/bin/env Rscript
# Compute Morris elementary effects (mu, mu*, sigma) from design-level metrics.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(yaml)
})

`%||%` <- function(a, b) if (is.null(a) || isTRUE(is.na(a))) b else a
as_logical_flag <- function(x) {
  if (is.null(x)) return(NA)
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x)) return(tolower(x) %in% c("true", "t", "yes", "y", "1"))
  as.logical(x)
}

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

design_candidates <- c(
  file.path(project_root, "workspace", "sensitivity", "morris_design_points.csv"),
  file.path(project_root, "workspace", "sensitivity", "config", "morris_design_points.csv")
)
runs_path <- file.path(project_root, "workspace", "sensitivity", "runs.csv")
config_dir <- file.path(project_root, "workspace", "sensitivity", "config", "generated")
results_dir <- file.path(project_root, "outputs", "sensitivity", "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
metrics_rds <- file.path(results_dir, "morris_design_metrics_long.rds")
metrics_csv <- file.path(results_dir, "morris_design_metrics_long.csv")
fig_dir <- file.path(project_root, "outputs", "sensitivity", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

site_levels <- c(
  "",
  "oilGas",
  "oilGas;cutblocks",
  "oilGas;cutblocks;mining",
  "oilGas;cutblocks;mining;seismicLines"
)

param_bounds <- list(
  totalDisturbanceRate = c(min = 0.5, max = 3.0),
  clusterDistance = c(min = 500, max = 2000)
)

log_info <- function(...) message(sprintf(...))

read_design_csv <- function() {
  path <- design_candidates[file.exists(design_candidates)][1]
  if (is.na(path)) return(tibble())
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

parse_run_name <- function(run_name) {
  tibble(
    trajectory_id = suppressWarnings(as.integer(str_match(run_name, "t(\\d+)")[, 2])),
    point_index = suppressWarnings(as.integer(str_match(run_name, "p(\\d+)")[, 2]))
  )
}

design_from_config <- function(cfg_path) {
  if (!file.exists(cfg_path)) return(NULL)
  cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
  if (is.null(cfg)) return(NULL)
  run_name <- cfg$run_name %||% cfg$scenario_id %||% basename(cfg_path)
  params <- cfg$params$anthroDisturbance_Generator %||% list()
  parsed <- parse_run_name(run_name)
  tibble(
    scenario_id = run_name,
    trajectory_id = parsed$trajectory_id,
    point_index = parsed$point_index,
    description = cfg$description %||% NA_character_,
    totalDisturbanceRate = params$totalDisturbanceRate %||% NA_real_,
    clusterDistance = params$clusterDistance %||% NA_real_,
    useClusterMethod = params$useClusterMethod %||% NA,
    siteSelectionAsDistributing = params$siteSelectionAsDistributing %||% NA_character_,
    config_file = cfg_path
  )
}

load_runs_registry <- function() {
  if (!file.exists(runs_path)) return(tibble())
  suppressMessages(readr::read_csv(runs_path, show_col_types = FALSE)) %>%
    filter(suite == "sensitivity")
}

load_design_metadata <- function() {
  design_csv <- read_design_csv()
  runs_df <- load_runs_registry()
  cfg_paths <- unique(runs_df$config_file)
  cfg_paths <- cfg_paths[file.exists(cfg_paths)]
  design_cfg <- map_dfr(cfg_paths, design_from_config)

  if (nrow(design_csv)) {
    design_df <- design_csv %>%
      mutate(
        scenario_id = dplyr::coalesce(
          if ("scenario_id" %in% names(design_csv)) scenario_id else NA_character_,
          if ("run_name" %in% names(design_csv)) run_name else NA_character_
        ),
        totalDisturbanceRate = dplyr::coalesce(
          if ("totalDisturbanceRate" %in% names(design_csv)) totalDisturbanceRate else NA_real_,
          if ("value_totalDisturbanceRate" %in% names(design_csv)) value_totalDisturbanceRate else NA_real_
        ),
        clusterDistance = dplyr::coalesce(
          if ("clusterDistance" %in% names(design_csv)) clusterDistance else NA_real_,
          if ("value_cluster_distance" %in% names(design_csv)) value_cluster_distance else NA_real_
        ),
        useClusterMethod = dplyr::coalesce(
          if ("useClusterMethod" %in% names(design_csv)) useClusterMethod else NA,
          if ("value_use_cluster_method" %in% names(design_csv)) value_use_cluster_method else NA
        ),
        siteSelectionAsDistributing = dplyr::coalesce(
          if ("siteSelectionAsDistributing" %in% names(design_csv)) siteSelectionAsDistributing else NA_character_,
          if ("value_site_selection" %in% names(design_csv)) value_site_selection else NA_character_
        ),
        trajectory_id = if ("trajectory_id" %in% names(design_csv)) trajectory_id else NA_integer_,
        point_index = if ("point_index" %in% names(design_csv)) point_index else NA_integer_,
        norm_totalDisturbanceRate = if ("norm_totalDisturbanceRate" %in% names(design_csv)) norm_totalDisturbanceRate else NA_real_,
        norm_clusterDistance = if ("norm_cluster_distance" %in% names(design_csv)) norm_cluster_distance else {
          if ("norm_clusterDistance" %in% names(design_csv)) norm_clusterDistance else NA_real_
        },
        norm_useClusterMethod = if ("norm_use_cluster_method" %in% names(design_csv)) norm_use_cluster_method else NA_real_,
        norm_siteSelection = if ("norm_site_selection" %in% names(design_csv)) norm_site_selection else NA_real_
      )
  } else {
    design_df <- tibble()
  }

  if (nrow(design_cfg)) {
    design_cfg <- design_cfg %>%
      mutate(
        norm_totalDisturbanceRate = (totalDisturbanceRate - param_bounds$totalDisturbanceRate[["min"]]) /
          diff(param_bounds$totalDisturbanceRate),
        norm_clusterDistance = (clusterDistance - param_bounds$clusterDistance[["min"]]) /
          diff(param_bounds$clusterDistance),
        norm_useClusterMethod = ifelse(is.na(useClusterMethod), NA_real_, as.numeric(as_logical_flag(useClusterMethod))),
        norm_siteSelection = {
          idx <- match(siteSelectionAsDistributing, site_levels) - 1
          ifelse(is.na(idx), NA_real_, idx / (length(site_levels) - 1))
        }
      )
  }

  merged <- bind_rows(
    design_df,
    design_cfg %>% filter(!scenario_id %in% design_df$scenario_id)
  )

  has_design_id <- "design_id" %in% names(merged)
  merged <- merged %>%
    mutate(
      design_id = if (has_design_id) design_id else row_number(),
      useClusterMethod = as_logical_flag(useClusterMethod),
      norm_totalDisturbanceRate = ifelse(
        is.na(norm_totalDisturbanceRate) & !is.na(totalDisturbanceRate),
        (totalDisturbanceRate - param_bounds$totalDisturbanceRate[["min"]]) /
          diff(param_bounds$totalDisturbanceRate),
        norm_totalDisturbanceRate
      ),
      norm_clusterDistance = ifelse(
        is.na(norm_clusterDistance) & !is.na(clusterDistance),
        (clusterDistance - param_bounds$clusterDistance[["min"]]) /
          diff(param_bounds$clusterDistance),
        norm_clusterDistance
      ),
      norm_useClusterMethod = ifelse(
        is.na(norm_useClusterMethod) & !is.na(useClusterMethod),
        as.numeric(as_logical_flag(useClusterMethod)),
        norm_useClusterMethod
      ),
      norm_siteSelection = ifelse(
        is.na(norm_siteSelection) & !is.na(siteSelectionAsDistributing),
        {
          idx <- match(siteSelectionAsDistributing, site_levels) - 1
          ifelse(is.na(idx), NA_real_, idx / (length(site_levels) - 1))
        },
        norm_siteSelection
      )
    ) %>%
    arrange(trajectory_id, point_index, design_id) %>%
    mutate(design_id = row_number())
  merged
}

is_diff <- function(a, b, tol = 1e-9) {
  if (all(is.na(c(a, b)))) return(FALSE)
  if (is.numeric(a) && is.numeric(b)) {
    !isTRUE(all.equal(a, b, tolerance = tol))
  } else {
    !identical(as.character(a), as.character(b))
  }
}

infer_changed_parameter <- function(df, param_cols) {
  df <- df %>% arrange(trajectory_id, step_index)
  out <- vector("character", nrow(df))
  for (i in seq_len(nrow(df))) {
    if (i == 1) {
      out[[i]] <- NA_character_
      next
    }
    prev <- df[i - 1, param_cols, drop = FALSE]
    cur <- df[i, param_cols, drop = FALSE]
    diffs <- map_lgl(param_cols, ~ is_diff(cur[[.x]], prev[[.x]]))
    if (sum(diffs, na.rm = TRUE) == 1) {
      out[[i]] <- param_cols[which(diffs)[1]]
    } else {
      out[[i]] <- NA_character_
    }
  }
  out
}

canonical_param <- function(x) {
  recode(tolower(x),
    "totaldisturbancerate" = "totalDisturbanceRate",
    "cluster_distance" = "clusterDistance",
    "clusterdistance" = "clusterDistance",
    "use_cluster_method" = "useClusterMethod",
    "useclustermethod" = "useClusterMethod",
    "site_selection" = "siteSelectionAsDistributing",
    "siteselection" = "siteSelectionAsDistributing",
    .default = x
  )
}

detect_changed <- function(r1, r2) {
  param_cols <- c("totalDisturbanceRate", "clusterDistance", "useClusterMethod_num", "siteSelection_code")
  diffs <- map_lgl(param_cols, function(col) is_diff(r1[[col]], r2[[col]]))
  if (sum(diffs, na.rm = TRUE) == 1) {
    param_cols[which(diffs)[1]]
  } else {
    NA_character_
  }
}

compute_ee_for_metric <- function(metric_id, yr, metrics_df, design_df, response_col = "value_median") {
  slice <- metrics_df %>%
    filter(metric_id == !!metric_id, year == !!yr) %>%
    select(design_id, trajectory_id, step_index, value = all_of(response_col))
  if (!nrow(slice)) return(tibble())
  merged <- slice %>%
    left_join(
      design_df %>%
        select(
          design_id, trajectory_id, step_index,
          totalDisturbanceRate, clusterDistance,
          useClusterMethod, useClusterMethod_num, siteSelection_code,
          norm_totalDisturbanceRate, norm_clusterDistance, norm_useClusterMethod, norm_siteSelection,
          changed_parameter
        ),
      by = c("design_id", "trajectory_id", "step_index")
    )
  trajs <- split(merged, merged$trajectory_id)
  map_dfr(trajs, function(df_traj) {
    df_traj <- df_traj %>% arrange(step_index)
    if (nrow(df_traj) < 2) return(tibble())
    map_dfr(seq_len(nrow(df_traj) - 1), function(i) {
      r1 <- df_traj[i, ]
      r2 <- df_traj[i + 1, ]
      if (is.na(r1$value) || is.na(r2$value)) return(tibble())
      param_changed <- canonical_param(r2$changed_parameter %||% r1$changed_parameter)
      if (is.na(param_changed)) {
        param_changed <- detect_changed(r1, r2)
        param_changed <- canonical_param(param_changed)
      }
      if (!param_changed %in% c("totalDisturbanceRate", "clusterDistance", "useClusterMethod", "useClusterMethod_num", "siteSelectionAsDistributing", "siteSelection_code")) {
        return(tibble())
      }
      norm_col <- switch(param_changed,
        totalDisturbanceRate = "norm_totalDisturbanceRate",
        clusterDistance = "norm_clusterDistance",
        useClusterMethod = "norm_useClusterMethod",
        useClusterMethod_num = "norm_useClusterMethod",
        siteSelectionAsDistributing = "norm_siteSelection",
        siteSelection_code = "norm_siteSelection",
        NA_character_
      )
      raw_col <- switch(param_changed,
        totalDisturbanceRate = "totalDisturbanceRate",
        clusterDistance = "clusterDistance",
        useClusterMethod = "useClusterMethod_num",
        useClusterMethod_num = "useClusterMethod_num",
        siteSelectionAsDistributing = "siteSelection_code",
        siteSelection_code = "siteSelection_code",
        NA_character_
      )
      dx <- r2[[norm_col]] - r1[[norm_col]]
      if (is.na(dx) || abs(dx) < 1e-12) {
        raw_dx <- r2[[raw_col]] - r1[[raw_col]]
        if (param_changed == "totalDisturbanceRate") {
          dx <- raw_dx / diff(param_bounds$totalDisturbanceRate)
        } else if (param_changed == "clusterDistance") {
          dx <- raw_dx / diff(param_bounds$clusterDistance)
        } else if (param_changed %in% c("siteSelectionAsDistributing", "siteSelection_code")) {
          dx <- raw_dx / (length(site_levels) - 1)
        } else {
          dx <- raw_dx
        }
      }
      if (is.na(dx) || abs(dx) < 1e-12) return(tibble())
      ee <- (r2$value - r1$value) / dx
      tibble(
        changed_param = if (param_changed == "siteSelection_code") "siteSelectionAsDistributing" else param_changed,
        metric_id = metric_id,
        year = yr,
        trajectory_id = r1$trajectory_id,
        ee = ee
      )
    })
  })
}

compute_effects <- function(metrics_df, design_df, response_col = "value_median") {
  metric_years <- metrics_df %>% distinct(metric_id, year)
  ee_long <- map2_dfr(metric_years$metric_id, metric_years$year, ~ compute_ee_for_metric(.x, .y, metrics_df, design_df, response_col))
  if (!nrow(ee_long)) return(list(ee_long = tibble(), effects = tibble()))
  effects <- ee_long %>%
    group_by(changed_param, metric_id, year) %>%
    summarise(
      mu = mean(ee, na.rm = TRUE),
      mu_star = mean(abs(ee), na.rm = TRUE),
      sigma = sd(ee, na.rm = TRUE),
      n_ee = sum(!is.na(ee)),
      .groups = "drop"
    ) %>%
    mutate(rank_mu_star = rank(-mu_star, ties.method = "average"))
  list(ee_long = ee_long, effects = effects)
}

write_plots <- function(effects) {
  if (!nrow(effects)) return(invisible(NULL))
  key_metrics <- effects %>%
    distinct(metric_id, year) %>%
    head(4)
  purrr::pwalk(key_metrics, function(metric_id, year) {
    df <- effects %>% filter(metric_id == !!metric_id, year == !!year)
    if (!nrow(df)) return(invisible(NULL))
    p <- ggplot(df, aes(x = mu_star, y = sigma, color = changed_param, label = changed_param)) +
      geom_point(size = 3, alpha = 0.8) +
      ggrepel::geom_text_repel(size = 3, max.overlaps = 10, show.legend = FALSE) +
      labs(
        title = paste0("Morris effects: ", metric_id, " (", year, ")"),
        x = expression(mu["*"]),
        y = expression(sigma),
        color = "Parameter"
      ) +
      theme_minimal()
    out <- file.path(fig_dir, sprintf("morris_%s_%s.png", metric_id, year))
    ggsave(out, plot = p, width = 7, height = 5, dpi = 150)
  })
}

main <- function() {
  metrics_df <- if (file.exists(metrics_rds)) {
    readRDS(metrics_rds)
  } else if (file.exists(metrics_csv)) {
    suppressMessages(readr::read_csv(metrics_csv, show_col_types = FALSE))
  } else {
    stop("Unable to locate design-level metrics (expected RDS or CSV).", call. = FALSE)
  }
  if (!nrow(metrics_df)) stop("Design-level metrics table is empty.", call. = FALSE)

  design_df <- load_design_metadata()
  if (!nrow(design_df)) stop("Design metadata could not be loaded.", call. = FALSE)

  design_df <- design_df %>%
    mutate(
      step_index = if ("point_index" %in% names(design_df)) point_index else step_index,
      useClusterMethod_num = as.numeric(as_logical_flag(useClusterMethod)),
      siteSelection_code = match(siteSelectionAsDistributing, site_levels) - 1
    )
  param_cols <- c("totalDisturbanceRate", "clusterDistance", "useClusterMethod_num", "siteSelection_code")
  if (!"changed_parameter" %in% names(design_df) || all(is.na(design_df$changed_parameter))) {
    design_df <- design_df %>%
      group_by(trajectory_id) %>%
      mutate(changed_parameter = infer_changed_parameter(cur_data_all(), param_cols)) %>%
      ungroup()
  }

  response_col <- if ("value_median" %in% names(metrics_df)) "value_median" else "value_mean"
  res <- compute_effects(metrics_df, design_df, response_col = response_col)
  ee_long <- res$ee_long
  effects <- res$effects

  effects_csv <- file.path(results_dir, "morris_effects_long.csv")
  effects_rds <- file.path(results_dir, "morris_effects_long.rds")
  if (nrow(effects)) {
    write_csv(effects, effects_csv)
    saveRDS(effects, effects_rds)
  }
  if (nrow(ee_long)) {
    write_csv(ee_long, file.path(results_dir, "morris_elementary_effects_long.csv"))
    saveRDS(ee_long, file.path(results_dir, "morris_elementary_effects_long.rds"))
  }
  write_plots(effects)

  ranked <- effects %>%
    group_by(changed_param) %>%
    summarise(median_mu_star = median(mu_star, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(median_mu_star))

  log_info("Analysed %d metrics (%d EE rows). Top parameters by median mu*: %s",
           effects %>% distinct(metric_id, year) %>% nrow(),
           nrow(ee_long),
           paste(ranked$changed_param, collapse = ", "))
  log_info("Effects saved to %s", effects_csv)
}

tryCatch(main(), error = function(e) {
  message("analyse_morris_effects.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
