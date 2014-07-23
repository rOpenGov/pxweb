# Testing the functions in the R package sweSCB:
# file: getMetaData.R
# require(testthat)
# test_file("inst/tests/tests.R")
# test_package("sweSCB")

cat("getMetaData : ")

test_that(desc="getMetadata works",{
  testFile<-scbGetMetadata()
  expect_that(testFile,is_a("data.frame"))
  expect_that(dim(testFile),is_equivalent_to(c(21,4)))

  testFile<-scbGetMetadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-scbGetMetadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV0119/IVPKNLonAr")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-scbGetMetadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNMan")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-scbGetMetadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-scbGetMetadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-scbGetMetadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ImpTotalKNMan")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
})

cat("\n")
