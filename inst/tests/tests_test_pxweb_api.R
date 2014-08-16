# Test suite for test_pxweb_api()

cat("\ntests_test_pxweb_api.R : ")

api_tests_test_pxweb_api <- list(
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK",
    test_dim = c(11, 8)
    ),
  
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/KU",
    test_dim = c(16, 8)
  )
)

test_that(desc="test_pxweb_api()",{    
  for (test in api_tests_test_pxweb_api){
    expect_that({
      test_data <- suppressMessages(test_pxweb_api(url=test$url))}, 
      not(throws_error()),
      info = test$url)
    
    expect_equal(object=dim(test_data), test$test_dim, info=test$url)
  }
})
