# Pipeline entry for the anthropogenic disturbance thesis project.
# TODO: finalize targets graph once modules are wired in.

tlibrary <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf('Package %s is not installed; please restore renv.', pkg), call. = FALSE)
  }
}

tlibrary('targets')

tar_option_set(packages = c('targets'))

list(
  tar_target(name = placeholder_setup, command = 'TODO: define initial target graph')
)
