# Testing the functions in the R package pxweb:
# file: get_pxweb_metadata.R
# require(testthat)
# test_file("inst/tests/tests.R")
# test_package("pxweb")

cat("\n\n API tests for Statistics Sweden:\n")
cat("get_pxweb_metadata : ")

test_that(desc="get_pxweb_metadata works",{
  testFile <- get_pxweb_metadata(baseURL = base_url("sweSCB", "v1", "sv"))
  expect_that(testFile,is_a("data.frame"))
  expect_that(dim(testFile),is_equivalent_to(c(21,4)))

  testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV0119/IVPKNLonAr")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNMan")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
  testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ImpTotalKNMan")
  expect_is(object=testFile$variables$variables[[1]], "list")
  
})

cat("\n")
