# Test suite for doing multiple downloads from the SCB api

# Tests to run multiple queries (calls)
context("multiple_queries_data.R")



test_that(desc="multiple data calls",{  
  skip("Until next version")
  skip_on_cran()
  
  api_tests_multiple_data <- list(
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
      dims = list(ContentsCode = c('PR0101A1'),
                  Tid = c('2001')),
      clean = FALSE)
  )
  
  api_config <- pxweb::api_parameters(url=api_tests_multiple_data[[1]]$url)[[1]]
  Sys.sleep(time=api_config$period_in_seconds)
  api_file <- paste(tempdir(), "api_time_stamp.Rdata", sep="/")
  if(file.exists(api_file)) file.remove(api_file)
  
  for (test in api_tests_multiple_data){
    api_config <- pxweb::api_parameters(url=test$url)[[1]]
    
    expect_that({
      for(i in 1:(api_config$calls_per_period + 10)){
        test_data <- 
          get_pxweb_data(url = test$url, dims = test$dims, clean = test$clean)
        }
      }, not(throws_error()),
      info = test$url)
  }
})


test_that(desc="multiple metadata calls",{  
  skip("Until next version")
  skip_on_cran()
  
  api_tests_multiple_metadata <- list(
    pxweb::base_url("api.scb.se", version = "v1", language = "sv")
  )
  
  for (test in api_tests_multiple_metadata){
    api_config <- pxweb::api_parameters(url=test)[[1]]
    
    expect_that({
      for(i in 1:(api_config$calls_per_period + 10)){
        topnode <- pxweb::get_pxweb_metadata(baseURL = test)
      }
    }, not(throws_error()),
    info = test)
  }
})



