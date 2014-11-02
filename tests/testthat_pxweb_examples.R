library(testthat)
Sys.sleep(pxweb::api_parameters()[["scb"]]$period_in_seconds)
test_check("pxweb", filter = "pxweb_examples")
