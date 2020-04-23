# Test suits for the examples in the documentation

context("get_pxweb_levels.R")

test_that(desc="get_pxweb_levels",{
  
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  expect_warning(
    lev <- get_pxweb_levels(baseURL = 
      paste0(pxweb_api$new("api.scb.se")$base_url(language = "sv"), "/ssd")), regexp = "deprecated")
  
})  





