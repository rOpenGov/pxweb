# Test suits for the base_url in the documentation

context("base_url.R")

test_that(desc="base_url()",{

  expect_that({ 
    test_data <- get_pxweb_metadata(base_url("api.scb.se", version = "v1", lang = "sv"))
  }, not(throws_error()))
  
  expect_that({ 
    test_data <- get_pxweb_metadata(base_url("api.scb.se", version = "v1", lang = "en"))
  }, not(throws_error()))
})  





