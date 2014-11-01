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

test_seeds <- c(as.integer(Sys.time()), 1408310599)

# test <- api_tests_test_pxweb_api[[1]]

test_that(desc="test_pxweb_api()",{    
  for (test in api_tests_test_pxweb_api){
    for (seed in test_seeds){
      expect_that({
        test_data <- test_pxweb_api(url=test$url, seed=seed)}, 
        not(throws_error()),
        info = paste(test$url, ", seed ", seed, sep=""))
      
      expect_equal(object=dim(test_data[[1]]), test$test_dim, info=test$url)
    }
  }
})
