# Test suite for big queries

context("big_queries.R")

test_that(desc="big queries",{  
  skip("Skip temporarily (until new version)")
  skip_on_cran()
  
  api_tests_big_query <- list(
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('*'), 
                  Civilstand = c('*'), 
                  Alder = c('*'), 
                  Kon = c('*'), 
                  ContentsCode = c('*'),
                  Tid = as.character(1970:1971)),
      clean=TRUE),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('*'), 
                  Civilstand = c('*'), 
                  Alder = c('*'), 
                  Kon = c('*'), 
                  ContentsCode = c('*'),
                  Tid = as.character(1970)),
      clean=FALSE)
  )
  test <- api_tests_big_query[[1]]
  for (test in api_tests_big_query){
    expect_that({
      test_data <- suppressMessages(
        get_pxweb_data(url = test$url,
                       dims = test$dims,
                       clean = test$clean)
        )
      }, 
      not(throws_error()),
      info = test$url)
    
    test_dim_size <- 
      pxweb:::calculate_data_dim(dim_length=pxweb:::get_dim_size(url = test$url, dims=test$dims)[[1]], 
                                 clean=test$clean)
    expect_equal(object=dim(test_data), test_dim_size, info=test$url)
    expect_equal(object=class(test_data), "data.frame", info=test$url)
    rm(test_data)
  }
})

