# Test suits for the base_url in the documentation

context("base_url.R")

test_that(desc="base_url()",{
  
  skip_on_cran()

  expect_that({ 
    test_data <- get_pxweb_metadata(base_url("api.scb.se", version = "v1", language = "sv"))
  }, not(throws_error()))
  
  expect_that({ 
    test_data <- get_pxweb_metadata(base_url("api.scb.se", version = "v1", language = "en"))
  }, not(throws_error()))
  
})  





