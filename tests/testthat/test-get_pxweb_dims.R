# Test suits for the examples in the documentation

context("get_pxweb_dims.R")

test_that(desc="Examples in get_pxweb()",{
  
  expect_that({ 
    bottom_node <- get_pxweb_metadata("http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv")
    dims <- suppressMessages(get_pxweb_dims(bottom_node))
  }, not(throws_error()))

})  





