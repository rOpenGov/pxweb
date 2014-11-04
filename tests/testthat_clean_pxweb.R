library(testthat)
library(stringr)
Sys.sleep(pxweb::api_parameters()[["scb"]]$period_in_seconds)
test_check("pxweb", filter = "clean_pxweb")
