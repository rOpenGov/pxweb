# Test suits for the examples in the documentation

context("get_pxweb_levels.R")

test_that(desc="get_pxweb_levels",{
  
  expect_warning(
    lev <- get_pxweb_levels(baseURL = 
      paste0(pxweb_api$new("api.scb.se")$base_url(language = "sv"), "/ssd")), regexp = "deprecated")
  
})  





