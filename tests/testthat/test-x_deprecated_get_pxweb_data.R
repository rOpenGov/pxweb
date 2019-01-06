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
  pxweb:::pxweb_clear_cache()

  api_tests_get_pxweb_data <- list(
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
      dims = list(ContentsCode = c('PR0101A1'),
                  Tid = c('1995', '1996', '1997')),
      clean = TRUE,
      test_dim = c(NA, 3),
      test_sum = 108200),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('00', '01'),
                  Civilstand = c('*'),
                  Alder = c('0', 'tot'),
                  Kon = c('*'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2010', '2011', '2012', '2013')),
      clean = TRUE,
      test_dim = c(128, 7),
      test_sum = 47107124),
    
    list(
      url="http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('00', '01'),
                  Civilstand = c('*'),
                  Alder = c('0', 'tot'),
                  Kon = c('*'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2010', '2011', '2012', '2013')),
      clean = FALSE,
      test_dim = c(32, 8),
      test_sum = NA), 
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv",
      dims = list(SNI2007 = c('*'),
                  ContentsCode = c('*'),
                  Tid = c('*')),
      clean = FALSE,
      test_dim = c(NA, NA),
      test_sum = NA),
    
    # Test swedish letters
    list( 
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0104/ME0104C/ME0104T3",
      dims = list(Region = c('*'),
                  Partimm = c('M','C','FP','KD','MP','S','V','SD','\u00D6VRIGA'),
                  ContentsCode = c('ME0104B7'),
                  Tid = c('2010')),
      clean = TRUE,
      test_dim = c(2907, 5),
      test_sum = 31999.3),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001S/SnabbStatTK1001",
      dims = list("ContentsCode" = c("TK1001AE"),
                  "Tid" = c("2014M02")
      ),
      clean = TRUE,
      test_sum = 18.3
    ),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('2584'),
                  Civilstand = c('*'),
                  Alder = c('1'),
                  Kon = c('1'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2017')),
      clean = TRUE,
      test_dim = c(NA, NA),
      test_sum = 144)  
    
  )
  
  
  for (i in seq_along(api_tests_get_pxweb_data)){
#    if(test$url == "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0401/BE0401A/BefolkprognRev2014") {
#      skip("Known error: comma bug in csv files")}

#    skip("Skip temporarily (until new version)")
    test <- api_tests_get_pxweb_data[[i]]
    expect_warning(
      test_data <- 
        get_pxweb_data(url = test$url,
                       dims = test$dims,
                       clean = test$clean))

    test_dim_size <- suppressWarnings(pxweb:::calculate_data_dim(pxweb:::get_dim_size(url = test$url, dims=test$dims)[[1]], test$clean))
    expect_equal(object=dim(test_data), test_dim_size, info=test$url)
    expect_equal(object=class(test_data), "data.frame", info=test$url)     
    if(!is.na(test$test_sum)){
      expect_equal(sum(test_data$values, na.rm = TRUE), expected = test$test_sum, label = test$url)
    }
  }
})


test_that(desc="get_pxweb_data()",{  
  
  skip_on_cran()
  # PXWEB query 
  pxweb_query_url <- "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
  pxweb_query_list <- 
    list("Region"=c("00","01","0114","0115","0117","0120","0123"),
         "Civilstand"=c("OG","G"),
         "Alder"=c("0","1","2","3","4","5","6","7","8","9","10"),
         "Kon"=c("1","2"),
         "ContentsCode"=c("BE0101N1","BE0101N2"),
         "Tid"=c("1968","1969","1970","1971","1972"))
  
  # Download data 
  expect_silent(px_data <- 
    pxweb_get(url = pxweb_query_url,
              query = pxweb_query_list))
  expect_warning(pxd1 <- as.data.frame(px_data))
  
  expect_warning(pxd2 <- get_pxweb_data(url = pxweb_query_url, dims = pxweb_query_list, clean = TRUE, encoding = NULL))
  
  expect_equal(dim(pxd2)[1], dim(pxd1)[1]*2)
  expect_equal(dim(pxd2)[2], dim(pxd1)[2])
  
  pxd2pop <- pxd2[pxd2$ContentsCode == "Population", ]
  pxd1pop <- pxd1[, 1:6]
  pxd2grw <- pxd2[pxd2$ContentsCode == "Population growth", ]
  pxd1grw <- pxd1[, c(1:5, 7)]

  expect_equal(sum(pxd1pop$Population, na.rm = TRUE), 
               sum(pxd2pop$values, na.rm = TRUE))
  expect_equal(sum(pxd1grw$`Population growth`, na.rm = TRUE), 
               sum(pxd2grw$values, na.rm = TRUE))
  expect_true(all(pxd1pop$Population >= 0 ))
  expect_true(all(pxd2pop$values >= 0))
  
})

test_that(desc="get_pxweb_data()",{  
  
  skip_on_cran()
  # PXWEB query 
  pxweb_query_url <- "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
  pxweb_query_list <- 
    list("Region"=c("00","01","0114","0115","0117","0120","0123"),
         "Civilstand"=c("OG","G"),
         "Alder"=c("0","1","2","3","4","5","6","7","8","9","10"),
         "Kon"=c("1","2"),
         "ContentsCode"=c("BE0101N1"),
         "Tid"=c("1968","1969","1970","1971","1972"))
  
  # Download data 
  expect_silent(px_data <- 
                  pxweb_get(url = pxweb_query_url,
                            query = pxweb_query_list))
  expect_silent(pxd1 <- as.data.frame(px_data))
  
  expect_warning(pxd2 <- get_pxweb_data(url = pxweb_query_url, dims = pxweb_query_list, clean = TRUE, encoding = NULL))
  
  expect_equal(dim(pxd2)[1], dim(pxd1)[1])
  expect_equal(dim(pxd2)[2]-1, dim(pxd1)[2])  
  
  expect_equal(sum(pxd1$Population, na.rm = TRUE), 
               sum(pxd2$values, na.rm = TRUE))
  expect_true(all(pxd1$Population >= 0 ))
  expect_true(all(pxd2$values >= 0))
  expect_true(all(table(pxd1$Population) == table(pxd2$values)))
  
})
