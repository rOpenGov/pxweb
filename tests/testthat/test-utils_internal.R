# Test suite for utils functions

context("utils_internal.R")


api_file <- paste(tempdir(), "api_time_stamp.Rdata", sep="/")
if(file.exists(api_file)) file.remove(api_file)

test_that(desc="api_timer()",{  
  
  expect_that({
    res <-
      system.time(
        for(i in 1:4){
          pxweb:::api_timer(api_url="http://foo.bar/")     
        })}, 
    not(throws_error()))
  
  expect_more_than(object=res[3],expected=4)
})

if(file.exists(api_file)) file.remove(api_file)



api_tests_create_batch_list <- list(
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
    dims = list(ContentsCode = c('*'),
                 Tid = c('*'))
    )
)


test_that(desc="create_batch_list()",{  
  for (test in api_tests_create_batch_list){
  expect_that({
    res <- create_batch_list(url=test$url, dims=test$dims)
      }, 
    not(throws_error()))
  }
})


