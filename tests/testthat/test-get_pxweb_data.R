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
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/en/ssd/UF/UF0536/Fullfoljt",
      dims = list(Studresultat = c('3'),
                  Kon = c('1'),
                  UtlBakgrund = c('1'),
                  Program = c('31'),
                  ContentsCode = c('UF0536A1'),
                  Tid = c('2007')),
      clean = FALSE,
      test_dim = c(NA, NA)),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI0814/MarkanvTatortZonArea",
      dims = list(Region = c('*'),
                  Kmzon = c('*'),
                  ArealStrandzon = c('*'),
                  ContentsCode = c('*'),
                  Tid = c('*')),
      clean = FALSE,
      test_dim = c(NA, NA)),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0539/Medb30",
      dims = list(Svarsalternativ='*',
                  Kon='*',
                  Studievag='*',
                  ContentsCode='*',
                  Tid='*'),
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
      url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/asas/010_asas_tau_101.px",
      dims = list(Alue = c("*"),
                  "Asuntokunnan koko" = c("*"),
                  Talotyyppi = c("S"),
                  Vuosi = c("*")
      ),
      clean = TRUE,
      test_dim = c(2568, NA)
    )  ,
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001S/SnabbStatTK1001",
      dims = list("ContentsCode" = c("TK1001AE"),
                  "Tid" = c("2014M02")
      ),
      clean = TRUE
    ),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI0814/MarkanvTatortZonArea",
      dims = list(Region = c('*'), Kmzon = c('*'), ArealStrandzon = c('*'), ContentsCode = c('*'), Tid = c('*')
      ),
      clean = TRUE
    ),

    list(
      url = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0401/BE0401A/BefolkprognRev2015",
      dims = list(Alder = c('0', '1', '2', '3', '4'),
                  Kon = c('1', '2'),
                  ContentsCode = c('0000008J'),
                  Tid = c('2015', '2016', '2017', '2018', '2019')),
      clean = FALSE,
      test_dim = c(NA, NA))  
    
  )
  
  
  for (test in api_tests_get_pxweb_data){
#    if(test$url == "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0401/BE0401A/BefolkprognRev2014") {
#      skip("Known error: comma bug in csv files")}

    skip("Skip temporarily (until new version)")
    
    expect_that({
      test_data <- 
        get_pxweb_data(url = test$url,
                       dims = test$dims,
                       clean = test$clean)}, 
      not(throws_error()),
      info = test$url)

    test_dim_size <- pxweb:::calculate_data_dim(pxweb:::get_dim_size(url = test$url, dims=test$dims)[[1]], test$clean)
    expect_equal(object=dim(test_data), test_dim_size, info=test$url)
    expect_equal(object=class(test_data), "data.frame", info=test$url)     
  }
})

test_that(desc="Test warnings",{  
  
  skip_on_cran()
  
  expect_that({
    test_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001S/SnabbStatTK1001"
    test_dims <- list("ContentsCode" = c("TK1001AE"), "Tid" = c("2014M02"))
    test_data <- 
      get_pxweb_data(url = test_url,
                     dims = test_dims,
                     clean = TRUE)}, 
    not(gives_warning()))
})


test_that(desc="Previous bug identified by Erik Bulow (#84 at github)",{  
  
  skip_on_travis()
  
  test_dims <-
    structure(
      list(
        Civilstand = "*", ContentsCode = "BE0101N1", Alder = c(
          "0",
          "1", "10", "11", "12", "14", "15", "16", "18", "19", "2", "20",
          "21", "22", "23", "24", "25", "26", "27", "28", "29", "3", "30",
          "31", "32", "33", "34", "35", "36", "37", "38", "39", "4", "40",
          "41", "42", "43", "44", "45", "46", "47", "48", "49", "5", "50",
          "51", "52", "53", "54", "55", "56", "57", "58", "59", "6", "60",
          "61", "62", "63", "64", "65", "66", "67", "68", "69", "7", "70",
          "71", "72", "73", "74", "75", "76", "77", "78", "79", "8", "80",
          "81", "82", "84", "85", "9"
        ), Kon = c("1", "2"), Tid = c(
          "2005",
          "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013",
          "2014"
        ), Region = c(
          "1382", "1383", "1384", "1401", "1402", "1407",
          "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439",
          "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447",
          "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470",
          "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485",
          "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493",
          "1494", "1495", "1496", "1497", "1498", "1499"
        )
      ), .Names = c("Civilstand",
                    "ContentsCode", "Alder", "Kon", "Tid", "Region")
    )  
  
  expect_that({
    test_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
    test_data <- 
      get_pxweb_data(url = test_url,
                     dims = test_dims,
                     clean = TRUE)}, 
    not(throws_error()))
})