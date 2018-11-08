# Test suits for the examples in the documentation

context("pxweb_test_api_endpoint")

test_that(desc="Mixed node levels object",{
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0301/"
  expect_output(res <- suppressWarnings(pxweb_test_api_endpoint(url)), regexp = "4 node.+and 14 table")
  expect_true(all(res$checked))
  expect_true(all(!res$error))
  
  url <- "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
  expect_output(res <- suppressWarnings(pxweb_test_api_endpoint(url, test_type = "touch")), regexp = "Table touched")
})  
