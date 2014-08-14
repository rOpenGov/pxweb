cat("\nclean_pxweb : ")

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