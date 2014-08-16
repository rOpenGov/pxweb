# Test suits for the examples in the documentation

cat("\ntests_get_pxweb_levels.R : ")

test_that(desc="Examples in get_pxweb()",{
  
  expect_that({ 
    lev <- get_pxweb_levels(baseURL = base_url("sweSCB", "v1", "sv"))
  }, not(throws_error()))
  
})  





