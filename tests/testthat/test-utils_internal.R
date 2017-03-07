# Test suite for utils functions

context("utils_internal.R")

test_that(desc="api_timer()",{  
  
  skip_on_cran()
  
  api_file <- paste(tempdir(), "api_time_stamp.Rdata", sep="/")
  if(file.exists(api_file)) file.remove(api_file)
  
  test_api <- 
    pxweb_api$new(api="foo.bar",
                  url="http://httpbin.org/[lang]/[version]",
                  description = "test api",
                  languages = "status",
                  versions = "404",           
                  calls_per_period = 1,
                  period_in_seconds = 2, 
                  max_values_to_download = 10)
  suppressMessages(test_api$write_to_catalogue())

  expect_that({
    res <-
      system.time(
        for(i in 1:4){
          pxweb:::api_timer(api_url="http://httpbin.org/")     
        })}, 
    not(throws_error()))
  
  expect_more_than(object=res[3],expected=4)
  
  if(file.exists(api_file)) file.remove(api_file)
})





test_that(desc="create_batch_list()",{  
  
  api_tests_create_batch_list <- list(
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
      dims = list(ContentsCode = c('*'),
                  Tid = c('*'))
    )
  )
  
  for (test in api_tests_create_batch_list){
    expect_that({
      res <- pxweb:::create_batch_list(url=test$url, dims=test$dims)
        }, 
      not(throws_error()))
  }
})

