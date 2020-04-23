# Test suits for the examples in the documentation

context("pxweb_test_api")

test_that(desc="Mixed node levels object",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0301/"
  expect_output(res <- suppressWarnings(pxweb_test_api(url)), regexp = "4 node.+and 14 table")
  expect_true(all(res$checked))
  # expect_true(all(!res$error)) # FIXME this also fails
  
  url <- "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
  tryr <- try(httr::GET(url), silent = TRUE)
  if(!inherits(tryr, "try-error")){
    expect_output(res <- suppressWarnings(pxweb_test_api(url, test_type = "touch")), regexp = "Table touched")
  }
  
  expect_silent(api_paths <- pxweb:::pxweb_test_create_api_paths(apis = pxweb_api_catalogue()))
  expect_true(all(c("http://api.scb.se/OV0104/v1/doris/en", 
                    "http://api.scb.se/OV0104/v1/doris/sv", 
                    "http://pxnet2.stat.fi/PXWeb/api/v1/en",
                    "http://pxnet2.stat.fi/PXWeb/api/v1/fi",
                    "http://pxnet2.stat.fi/PXWeb/api/v1/sv") %in% api_paths$paths))
})  
