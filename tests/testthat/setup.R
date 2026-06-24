library(httptest)

pxweb_env_flag <- function(name) {
  tolower(Sys.getenv(name, "false")) %in% c("true", "1", "yes", "y")
}

pxweb_run_live_tests <- function() {
  pxweb_env_flag("PXWEB_RUN_LIVE_TESTS")
}

pxweb_run_external_live_tests <- function() {
  pxweb_env_flag("PXWEB_RUN_EXTERNAL_LIVE_TESTS")
}

skip_if_not_live_api <- function() {
  if (!pxweb_run_live_tests()) {
    testthat::skip("Set PXWEB_RUN_LIVE_TESTS=true to run live API tests.")
  }
  skip_if_offline()
}

skip_if_not_external_live_api <- function() {
  if (!pxweb_run_live_tests() || !pxweb_run_external_live_tests()) {
    testthat::skip("Set PXWEB_RUN_LIVE_TESTS=true and PXWEB_RUN_EXTERNAL_LIVE_TESTS=true to run external live API tests.")
  }
  skip_if_offline()
}
