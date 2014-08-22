# Test suite for utils functions

cat("\ntests_utils_internal.R : ")


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


#' url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet"
#' dims <- list(ContentsCode = c('*'),
#'             Tid = c('*'))
#' \dontrun{
#' batches <- create_batch_list(url, dims)
#' }


