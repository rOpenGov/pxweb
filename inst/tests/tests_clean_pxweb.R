# Test suite for clean_pxweb()

cat("\ntests_clean_pxweb.R : ")

api_tests_clean_pxweb <- list(
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
    dims = list(Region = c('00', '01'),
                Civilstand = c('*'),
                Alder = c('0', 'tot'),
                Kon = c('*'),
                ContentsCode = c('BE0101N1'),
                Tid = c('2010', '2011', '2012', '2013'))
    )
)

test_that(desc="clean_pxweb",{
  for (test in api_tests_clean_pxweb){
    expect_that({
      test_data <-
        get_pxweb_data(url = test$url,
                       dims = test$dims,
                       clean = TRUE)
    }, not(throws_error()), info = test$url)

    expect_is(object=test_data[,ncol(test_data)], "numeric", info = test$url)
    
    for(j in 1:(ncol(test_data)-1)){
      expect_equal(object=sum(is.na(test_data[,j])), 0)
      expect_is(object=test_data[,j], "factor", 
                info=paste(test$url, " : col ", j, ".", sep=""))
    }
  }
})

