# Test suite for big queries

cat("\ntests_big_query.R : ")

api_tests_big_query <- list(
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
    dims = list(Region = c('*'), 
                Civilstand = c('*'), 
                Alder = c('*'), 
                Kon = c('*'), 
                ContentsCode = c('*'),
                Tid = c('*')))
  )

test_that(desc="big queries",{  
  for (test in api_tests_big_query){
    ptm <- proc.time()
    expect_that({
      test_data <- 
        get_pxweb(url = test$url,
                  dims = test$dims,
                  clean = FALSE)}, 
      not(throws_error()),
      info = test$url)
      diff <- proc.time()-ptm
      Sys.sleep(max(1.1-diff[3],0))
  }
})

