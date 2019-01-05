# Test suite for get_pxweb_data()

# Below is the tests that should be conducted as a list. 
# Each listelement is a named object that contains url and dims 
# that make up the call through get_pxweb_data().
# Test will be done that downloading works, that the function returns a data.frame and that
# the size of the data.frame is test_dim, if missing values the dimension is not tested.
# in test_dim. If NA in test_dim, the dimension is ignored.

context("get_pxweb_data.R")

test_that(desc="get_pxweb_data()",{  
  
  skip_on_cran()
  expect_silent(pxweb_clear_cache())

  api_tests_get_pxweb_data <- list(
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
      dims = list(ContentsCode = c('PR0101A1'),
                  Tid = c('*')),
      clean = TRUE,
      test_dim = c(NA, 3)),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('00', '01'),
                  Civilstand = c('*'),
                  Alder = c('0', 'tot'),
                  Kon = c('*'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2010', '2011', '2012', '2013')),
      clean = TRUE,
      test_dim = c(128, 7)),
    
    list(
      url="http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('00', '01'),
                  Civilstand = c('*'),
                  Alder = c('0', 'tot'),
                  Kon = c('*'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2010', '2011', '2012', '2013')),
      clean = FALSE,
      test_dim = c(32, 8)), 
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv",
      dims = list(SNI2007 = c('*'),
                  ContentsCode = c('*'),
                  Tid = c('*')),
      clean = FALSE,
      test_dim = c(NA, NA)),
    
    # Test swedish letters
    list( 
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0104/ME0104C/ME0104T3",
      dims = list(Region = c('*'),
                  Partimm = c('M','C','FP','KD','MP','S','V','SD','\u00D6VRIGA'),
                  ContentsCode = c('ME0104B7'),
                  Tid = c('2010')),
      clean = TRUE,
      test_dim = c(2907, 5)),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001S/SnabbStatTK1001",
      dims = list("ContentsCode" = c("TK1001AE"),
                  "Tid" = c("2014M02")
      ),
      clean = TRUE
    ),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('2584'),
                  Civilstand = c('*'),
                  Alder = c('1'),
                  Kon = c('1'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2017')),
      clean = FALSE,
      test_dim = c(NA, NA))  
    
  )
  
  
  for (test in api_tests_get_pxweb_data){
#    if(test$url == "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0401/BE0401A/BefolkprognRev2014") {
#      skip("Known error: comma bug in csv files")}

#    skip("Skip temporarily (until new version)")
    expect_warning(
      test_data <- 
        get_pxweb_data(url = test$url,
                       dims = test$dims,
                       clean = test$clean))

    test_dim_size <- suppressWarnings(pxweb:::calculate_data_dim(pxweb:::get_dim_size(url = test$url, dims=test$dims)[[1]], test$clean))
    expect_equal(object=dim(test_data), test_dim_size, info=test$url)
    expect_equal(object=class(test_data), "data.frame", info=test$url)     
  }
})
