library(testthat)
Sys.sleep(pxweb::api_parameters()[["api.scb.se"]]$period_in_seconds)
test_check("pxweb", filter = "interactive")
