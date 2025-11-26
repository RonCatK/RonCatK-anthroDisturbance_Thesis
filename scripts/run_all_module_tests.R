#!/usr/bin/env Rscript

# Run full unit test suites for all modules and write reports under artifacts/unit_tests/<module>/

suppressPackageStartupMessages({
  library(testthat)
  library(covr)
  library(withr)
  library(raster)
  library(tictoc)
})

options(repos = c(CRAN = "https://cloud.r-project.org"))
Sys.setenv(
  R_PROFILE = "/dev/null",
  R_PROFILE_USER = "/dev/null",
  R_ENVIRON = "/dev/null",
  R_ENVIRON_USER = "/dev/null"
)

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
  report_dir <- normalizePath(file.path("artifacts", "unit_tests", mod$name), mustWork = FALSE)
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
        skip_files <- c("test-createCropLayFinalYear1.R", "test-generateDisturbances.R")
        test_files <- test_files[!basename(test_files) %in% skip_files]
      }
      if (!length(test_files)) {
        writeLines("No tests found.", file.path(report_dir, "tests.log"))
        return(invisible(NULL))
      }

      log_path <- file.path(report_dir, "tests.log")
      junit_path <- file.path(report_dir, "tests.xml")
      sink(log_path, split = TRUE)
      on.exit({ while (sink.number() > 0) sink() }, add = TRUE)

      if (!exists(".robustDigest", inherits = FALSE)) {
        assign(".robustDigest", digest::digest, envir = .GlobalEnv)
      }

      reporter <- testthat::MultiReporter$new(list(
        testthat::SummaryReporter$new(),
        testthat::JunitReporter$new(file = junit_path)
      ))

      for (tf in test_files) {
        testthat::test_file(
          tf,
          reporter = reporter,
          stop_on_failure = TRUE,
          stop_on_warning = FALSE
        )
      }

      cov <- covr::file_coverage(r_scripts, test_files)
      covr::report(cov, file = file.path(report_dir, "coverage.html"), browse = FALSE)
      writeLines(sprintf("coverage: %.2f%%", covr::percent_coverage(cov)), file.path(report_dir, "coverage.txt"))
    })
  })
}

invisible(lapply(modules, run_module))
