# Test suite for utils functions

context("api_parameters.R")

test_that(desc="api_parameters()",{  
  api_tests_utils_api_parameters <- list(
    "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  )
  
  expect_that({
    api_all_conf <- api_parameters()}, 
    not(throws_error()))
  
  for (test in api_tests_utils_api_parameters){
    expect_that({
      api_url_conf <- api_parameters(test)}, 
      not(throws_error()),
      info = test)
  }
})
