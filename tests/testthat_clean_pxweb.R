library(testthat)
library(stringr)
Sys.sleep(pxweb::api_parameters()[["api.scb.se"]]$period_in_seconds)
test_check("pxweb", filter = "clean_pxweb")
