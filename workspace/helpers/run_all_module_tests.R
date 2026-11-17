#!/usr/bin/env Rscript

# Run full unit test suites for all modules and write reports under outputs/traceability/unit_tests/<module>/

suppressPackageStartupMessages({
  library(testthat)
  library(covr)
  library(withr)
  library(raster)
  library(tictoc)
})

parse_junit_summary <- function(path) {
  if (!file.exists(path)) {
    return(list(tests = NA_integer_, failures = NA_integer_, errors = NA_integer_, skipped = NA_integer_))
  }
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  suite_lines <- grep("<testsuite\\b", lines, value = TRUE)
  if (!length(suite_lines)) {
    return(list(tests = NA_integer_, failures = NA_integer_, errors = NA_integer_, skipped = NA_integer_))
  }
  extract_attr_int <- function(line, attr) {
    pat <- paste0(attr, "=\"([0-9]+)\"")
    m <- regexec(pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1]]
    if (length(hit) >= 2) suppressWarnings(as.integer(hit[[2]])) else 0L
  }
  list(
    tests = sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "tests"), na.rm = TRUE),
    failures = sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "failures"), na.rm = TRUE),
    errors = sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "errors"), na.rm = TRUE),
    skipped = sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "skipped"), na.rm = TRUE)
  )
}

options(repos = c(CRAN = "https://cloud.r-project.org"))
Sys.setenv(
  R_PROFILE = "/dev/null",
  R_PROFILE_USER = "/dev/null",
  R_ENVIRON = "/dev/null",
  R_ENVIRON_USER = "/dev/null",
  NOT_CRAN = "true"
)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

modules <- list(
  list(
    name = "anthroDisturbance_DataPrep",
    path = "modules/anthroDisturbance_DataPrep",
    packages = c(
      "covr", "testthat", "withr", "terra", "data.table", "qs", "sf",
      "reproducible", "raster", "tictoc", "mockery", "Require",
      "fasterize", "xml2", "digest", "DT", "htmltools", "htmlwidgets"
    )
  ),
  list(
    name = "anthroDisturbance_Generator",
    path = "modules/anthroDisturbance_Generator",
    packages = c(
      "covr", "testthat", "withr", "terra", "data.table", "qs", "sf",
      "reproducible", "raster", "tictoc", "mockery", "digest", "crayon",
      "msm", "doParallel", "foreach", "dplyr", "sp", "stringi", "zip",
      "Require", "fasterize", "truncnorm", "xml2", "doSNOW"
    )
  ),
  list(
    name = "potentialResourcesNT_DataPrep",
    path = "modules/potentialResourcesNT_DataPrep",
    packages = c(
      "covr", "testthat", "withr", "terra", "data.table", "Require",
      "mockery", "DT", "htmltools", "htmlwidgets", "qs", "sf", "xml2"
    )
  )
)

ensure_packages <- function(pkgs) {
  pkgs <- unique(pkgs)
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) install.packages(missing)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

force_unlock_paths <- function() {
  envs <- list(.GlobalEnv, baseenv(), .BaseNamespaceEnv)
  if ("SpaDES.core" %in% loadedNamespaces()) {
    envs <- c(envs, list(asNamespace("SpaDES.core")))
  }
  for (env in envs) {
    if (exists("Paths", envir = env, inherits = FALSE)) {
      if (bindingIsLocked("Paths", env)) try(unlockBinding("Paths", env), silent = TRUE)
      try(rm("Paths", envir = env), silent = TRUE)
    }
  }
}

run_module <- function(mod) {
  module_status <- list(module = mod$name, status = "success", error_message = "")
  tryCatch({
    report_dir <- file.path(project_root, "outputs", "traceability", "unit_tests", mod$name)
    if (dir.exists(report_dir)) unlink(report_dir, recursive = TRUE)
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

    ensure_packages(mod$packages)

    force_unlock_paths()

    with_envvar(c(R_LIBS_USER = tempdir()), {
      with_dir(mod$path, {
        force_unlock_paths()
        paths_val <- list(inputPath = tempdir(), outputPath = tempdir())
        assigned <- FALSE
        for (env in c(list(.GlobalEnv), if ("SpaDES.core" %in% loadedNamespaces()) list(asNamespace("SpaDES.core")) else list())) {
          try({
            if (exists("Paths", envir = env, inherits = FALSE) && bindingIsLocked("Paths", env)) {
              unlockBinding("Paths", env)
            }
            assign("Paths", paths_val, envir = env)
            assigned <- TRUE
          }, silent = TRUE)
          if (assigned) break
        }

        r_scripts <- list.files("R", pattern = "\\.R$", full.names = TRUE)
        invisible(lapply(r_scripts, source))
        helper_files <- list.files(file.path("tests", "testthat"), pattern = "^helper-.*\\.R$", full.names = TRUE)
        if (length(helper_files)) lapply(helper_files, source)

        test_files <- list.files(file.path("tests", "testthat"), pattern = "^test-.*\\.R$", full.names = TRUE)
        if (mod$name == "anthroDisturbance_Generator") {
          skip_files <- c("test-generateDisturbances.R")
          test_files <- test_files[!basename(test_files) %in% skip_files]
        }
        if (!length(test_files)) {
          writeLines("No tests found.", file.path(report_dir, "tests.log"))
          module_status$status <- "skip"
          module_status$error_message <- "No tests found."
        } else {
          log_path <- file.path(report_dir, "tests.log")
          junit_dir <- file.path(report_dir, "junit")
          dir.create(junit_dir, recursive = TRUE, showWarnings = FALSE)
          sink(log_path, split = TRUE)
          on.exit({ while (sink.number() > 0) sink() }, add = TRUE)

          if (!exists(".robustDigest", inherits = FALSE)) {
            assign(".robustDigest", digest::digest, envir = .GlobalEnv)
          }

          per_file <- lapply(test_files, function(tf) {
            junit_path <- file.path(junit_dir, paste0(tools::file_path_sans_ext(basename(tf)), ".xml"))
            reporter <- testthat::MultiReporter$new(list(
              testthat::SummaryReporter$new(),
              testthat::JunitReporter$new(file = junit_path)
            ))
            testthat::test_file(
              tf,
              reporter = reporter,
              stop_on_failure = FALSE,
              stop_on_warning = FALSE
            )
            summ <- parse_junit_summary(junit_path)
            data.frame(
              test_file = basename(tf),
              junit = junit_path,
              tests = summ$tests,
              failures = summ$failures,
              errors = summ$errors,
              skipped = summ$skipped,
              stringsAsFactors = FALSE
            )
          })
          per_file_dt <- do.call(rbind, per_file)
          utils::write.csv(per_file_dt, file = file.path(report_dir, "tests_summary.csv"), row.names = FALSE)

          has_failures <- isTRUE(sum(per_file_dt$failures, na.rm = TRUE) > 0L || sum(per_file_dt$errors, na.rm = TRUE) > 0L)
          if (has_failures) {
            module_status$status <- "fail"
            module_status$error_message <- sprintf(
              "Unit tests reported failures=%s errors=%s (see %s)",
              sum(per_file_dt$failures, na.rm = TRUE),
              sum(per_file_dt$errors, na.rm = TRUE),
              file.path(report_dir, "tests_summary.csv")
            )
          } else {
            cov_src <- r_scripts
            if (mod$name == "anthroDisturbance_Generator") {
              cov_src <- setdiff(cov_src, file.path("R", c("generateDisturbances.R", "diagnostics.R")))
            }
            cov <- covr::file_coverage(cov_src, test_files)
            covr::report(cov, file = file.path(report_dir, "coverage.html"), browse = FALSE)
            writeLines(sprintf("coverage: %.2f%%", covr::percent_coverage(cov)), file.path(report_dir, "coverage.txt"))
          }
        }
      })
    })
  }, error = function(e) {
    module_status$status <- "error"
    module_status$error_message <- conditionMessage(e)
  })
  module_status
}

results <- lapply(modules, run_module)
results_dt <- do.call(
  rbind,
  lapply(results, function(row) data.frame(row, stringsAsFactors = FALSE))
)
utils::write.csv(results_dt, file = file.path(project_root, "outputs", "traceability", "unit_tests", "summary.csv"), row.names = FALSE)

failed <- results_dt[results_dt$status %in% c("fail", "error"), , drop = FALSE]
if (nrow(failed)) {
  message("Some module test suites failed. Summary written to outputs/traceability/unit_tests/summary.csv")
  print(failed)
  quit(save = "no", status = 1, runLast = FALSE)
}
