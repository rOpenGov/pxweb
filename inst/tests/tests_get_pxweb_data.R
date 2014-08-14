# Test suite get_pxweb_data()

# Below is the tests that should be conducted as a list. 
# Each listelement is a named object that contains url and dims 
# that make up the call through get_pxweb_data().
# Test will be done that downloading works, that the function returns a data.frame and that
# the size of the data.frame is test_dim, if missing values the dimension is not tested.
# in test_dim. If NA in test_dim, the dimension is ignored.

cat("\nget_pxweb_data : ")

api_tests_get_pxweb_data <- list(
  list(
    url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
    dims = list(ContentsCode = c('PR0101A1'),
                Tid = c('*')),
    clean = TRUE,
    test_dim = c(NA, 3)),
  
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
    test_dim = c(NA, NA))
  
  # Add Bennys example here
)

# test <- api_tests_get_pxweb_data[[1]]

test_that(desc="get_pxweb_data()",{  
  for (test in api_tests_get_pxweb_data){
    ptm <- proc.time()
    expect_that({
      test_data <- 
        get_pxweb(url = test$url,
                  dims = test$dims,
                  clean = test$clean)}, 
      not(throws_error()),
      info = test$url)
    diff <- proc.time()-ptm
    Sys.sleep(max(1.1-diff[3],0))
    
    if(!is.na(test$test_dim[1])) expect_equal(object=nrow(test_data), test$test_dim[1])
    if(!is.na(test$test_dim[2])) expect_equal(object=ncol(test_data), test$test_dim[2])
    expect_equal(object=class(test_data), "data.frame")     
  }
})

# 
# 
# test_that(desc="get_pxweb_data()",{
#   expect_that({
#     test_data <- 
#       get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
#                 dims = list(Region = c('00', '01'),
#                             Civilstand = c('*'),
#                             Alder = c('0', 'tot'),
#                             Kon = c('*'),
#                             ContentsCode = c('BE0101N1'),
#                             Tid = c('2010', '2011', '2012', '2013')),
#                 clean = FALSE)
#   }, not(throws_error()))
#   diff <- proc.time()-ptm
#   Sys.sleep(max(1.1-diff[3],0))
#   
#   expect_equal(object=dim(testData), c(32,8))
#   expect_equal(object=class(testData), "data.frame")
# 
# })
# 
# 
# test_that(desc="get_pxweb",{
#   ptm <- proc.time()
#   expect_that({
#     testData <- 
#       get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
#                 dims = list(Region = c('00', '01'),
#                              Civilstand = c('*'),
#                              Alder = c('0', 'tot'),
#                              Kon = c('*'),
#                              ContentsCode = c('BE0101N1'),
#                              Tid = c('2010', '2011', '2012', '2013')),
#                  clean = FALSE)
#   }, not(throws_error()))
#   diff <- proc.time()-ptm
#   Sys.sleep(max(1.1-diff[3],0))
#   
#   expect_equal(object=dim(testData), c(32,8))
#   expect_equal(object=class(testData), "data.frame")
#   
#   ptm <- proc.time()
#   expect_that({
#     testData2 <- 
#       get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
#                  dims = list(ContentsCode = c('PR0101A1'),
#                              Tid = c('*')),
#                  clean = TRUE)
#   }, not(throws_error()))
#   diff <- proc.time()-ptm
#   Sys.sleep(max(1.1-diff[3],0))  
# 
#   expect_equal(object=class(testData2), "data.frame")
#   expect_equal(object=ncol(testData2), 3)
# })


