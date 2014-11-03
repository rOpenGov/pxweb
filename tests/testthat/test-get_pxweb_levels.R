# Test suits for the examples in the documentation

context("get_pxweb_levels.R")

test_that(desc="Examples in get_pxweb()",{
  
  expect_that({ 
    lev <- get_pxweb_levels(baseURL = 
      paste0(pxweb_api$new("scb")$base_url(language = "sv"), "/ssd"))
  }, not(throws_error()))
  
})  





