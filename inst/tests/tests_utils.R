# Test suite for big queries

cat("\ntests_utils.R : ")

api_tests_utils_api_parameters <- list(
  "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
)

test_that(desc="api_parameters()",{  
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


test_that(desc="api_timer()",{  
  
  expect_that({
    res <-
      system.time(
        for(i in 1:4){
          api_timer(api_url="http://foo.bar/")      
        })}, 
    not(throws_error()))
  
  expect_more_than(object=res[3],expected=4)
})


