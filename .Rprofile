# Set the test repository as standard repository such that we test against the development versions
options(
  repos = c(options()$repos, "NN Test" = paste0(Sys.getenv("pacman_url"), "/test-internal/latest")
  )
)
# Make sure that the development library is present
if (!identical(Sys.getenv("ON_DEVOPS"), "TRUE")) {
  .my_library <- file.path("..", "library", paste0(getRversion()))
  dir.create(.my_library, showWarnings = FALSE, recursive = TRUE)
  if (interactive()) {
    cat("Intalling packages to: \n")
    cat(.libPaths()[1], "\n\n")
  }
}
