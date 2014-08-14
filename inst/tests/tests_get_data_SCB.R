# Testing the functions in the R package pxweb:
# require(testthat)
# test_file("inst/tests/tests_get_pxweb.R")
# load("inst/tests/testFiles.Rdata")
# test_package("pxweb")


cat("\n\n API tests for Statistics Sweden:\n")
cat("get_pxweb : ")

test_that(desc="get_pxweb",{
  ptm <- proc.time()
  expect_that({
    testData <- 
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                 dims = list(Region = c('00', '01'),
                             Civilstand = c('*'),
                             Alder = c('0', 'tot'),
                             Kon = c('*'),
                             ContentsCode = c('BE0101N1'),
                             Tid = c('2010', '2011', '2012', '2013')),
                 clean = FALSE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))
  
  expect_equal(object=dim(testData), c(32,8))
  expect_equal(object=class(testData), "data.frame")
  
  ptm <- proc.time()
  expect_that({
    testData2 <- 
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
                 dims = list(ContentsCode = c('PR0101A1'),
                             Tid = c('*')),
                 clean = TRUE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))  

  expect_equal(object=class(testData2), "data.frame")
  expect_equal(object=ncol(testData2), 3)
})

cat("\n.clean_pxweb : ")

test_that(desc="get_pxweb",{
  ptm <- proc.time()
  expect_that({
    cleanTestData <-
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                 dims = list(Region = c('00', '01'),
                             Civilstand = c('*'),
                             Alder = c('0', 'tot'),
                             Kon = c('*'),
                             ContentsCode = c('BE0101N1'),
                             Tid = c('2010', '2011', '2012', '2013')),
                 clean = TRUE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))
  
  expect_equal(object=dim(cleanTestData), c(128,7))
  expect_equal(object=class(cleanTestData), "data.frame")
  expect_equal(object=sum(is.na(cleanTestData[,1])), 0)
  expect_is(object=cleanTestData[,1], "factor")
  expect_equal(object=sum(is.na(cleanTestData[,2])), 0)
  expect_is(object=cleanTestData[,2], "factor")
  expect_equal(object=sum(is.na(cleanTestData[,3])), 0)
  expect_is(object=cleanTestData[,3], "factor")
  expect_equal(object=sum(is.na(cleanTestData[,4])), 0)
  expect_is(object=cleanTestData[,4], "factor")
  
})

test_that(desc="Examples",{
  ptm <- proc.time()
  expect_that({
    cleanTestData <-
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv",
                 dims = list(SNI2007 = c('*'),
                             ContentsCode = c('*'),
                             Tid = c('*')),
                 clean = TRUE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))
  
  ptm <- proc.time()
  expect_that({ 
    cleanTestData <-
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI0814/MarkanvTatortZonArea",
                 dims = list(Region = c('*'),
                             Kmzon = c('*'),
                             ArealStrandzon = c('*'),
                             ContentsCode = c('*'),
                             Tid = c('*')),
                 clean = TRUE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))
  
  ptm <- proc.time()
  expect_that({ 
    cleanTestData <-
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0539/Medb30",
                 dims = list(Svarsalternativ='*',
                             Kon='*',
                             Studievag='*',
                             ContentsCode='*',
                             Tid='*'),
                 clean = TRUE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))
})  


cat("\nBig query tests : ")
test_that(desc="Big query",{
  ptm <- proc.time()
   expect_that({
     cleanTestData <-
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                 dims = list(Region = c('*'), Civilstand = c('*'), Alder = c('*'), Kon = c('*'), ContentsCode = c('*'),Tid = c('*')),
                 clean = FALSE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))
})  
