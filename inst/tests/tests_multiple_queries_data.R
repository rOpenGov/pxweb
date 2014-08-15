# Test suite for doing multiple downloads from the SCB api

# Tests to run multiple queries (calls)
cat("\ntests_multiple_queries_data.R : ")


api_tests_multiple_data <- list(
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
    dims = list(ContentsCode = c('PR0101A1'),
                Tid = c('2001')),
    clean = FALSE)
)

test_that(desc="multiple data calls",{  
  for (test in api_tests_multiple_data){
    api_config <- pxweb::api_parameters(url=test$url)
    
    expect_that({
      for(i in 1:(api_config$calls_per_period * 2)){
        test_data <- 
          get_pxweb_data(url = test$url, dims = test$dims, clean = test$clean)
        }
      },not(throws_error()),
      info = test$url)
  }
})


api_tests_multiple_metadata <- list(
  pxweb::base_url("sweSCB", "v1", "sv")
)

test_that(desc="multiple metadata calls",{  
  for (test in api_tests_multiple_metadata){
    api_config <- pxweb::api_parameters(url=test)
    
    expect_that({
      for(i in 1:(api_config$calls_per_period * 2)){
        topnode <- pxweb::get_pxweb_metadata(baseURL = baseURL)
      }
    }, not(throws_error()),
    info = test)
  }
})

api_config <- pxweb::api_parameters(url=api_tests_multiple_metadata[[1]])
Sys.sleep(time=api_config$period_in_seconds)

