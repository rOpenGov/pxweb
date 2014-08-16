# Test suite for big queries

cat("\ntests_big_queries.R : ")

api_tests_big_query <- list(
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
    dims = list(Region = c('*'), 
                Civilstand = c('*'), 
                Alder = c('*'), 
                Kon = c('*'), 
                ContentsCode = c('*'),
                Tid = as.character(1970:1971)),
    clean=TRUE,
    test_dim = c(979200, 7)),
  
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
    dims = list(Region = c('*'), 
                Civilstand = c('*'), 
                Alder = c('*'), 
                Kon = c('*'), 
                ContentsCode = c('*'),
                Tid = as.character(1970)),
    clean=FALSE,
    test_dim = c(248880, 6))
  )

test_that(desc="big queries",{  
  for (test in api_tests_big_query){
    expect_that({
      test_data <- 
        get_pxweb_data(url = test$url,
                       dims = test$dims,
                       clean = test$clean)}, 
      not(throws_error()),
      info = test$url)
    
    if(!is.na(test$test_dim[1])) expect_equal(object=nrow(test_data), test$test_dim[1], info=test$url)
    if(!is.na(test$test_dim[2])) expect_equal(object=ncol(test_data), test$test_dim[2], info=test$url)
    rm(test_data)
  }
})

