# Test suits for the examples in the documentation

context("get_pxweb_dims.R")

test_that(desc="get_pxweb_dims()",{
  
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  expect_warning(bottom_node <- get_pxweb_metadata("http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv"), regexp = "deprecated")
  expect_warning(dims <- suppressMessages(get_pxweb_dims(bottom_node)), regexp = "deprecated")

})  





