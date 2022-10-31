# Functions only used for testing

on_github_actions <- function() identical(Sys.getenv("GITHUB_ACTIONS"), "true")

get_root_path <- function() {
  x <- getwd()
  if (on_github_actions()) x <- Sys.getenv("GITHUB_WORKSPACE")
  x
}
