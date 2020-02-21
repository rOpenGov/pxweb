# Test suite for utils functions

context("api_parameters.R")

test_that(desc="api_parameters()",{  
  api_tests_utils_api_parameters <- list(
    "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  )
  
  expect_warning(api_all_conf <- api_parameters())
  
  for (test in api_tests_utils_api_parameters){
    expect_warning(
      api_url_conf <- api_parameters(test),
      info = test)
  }
})
