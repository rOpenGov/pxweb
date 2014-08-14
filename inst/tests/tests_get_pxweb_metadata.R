# Test suite for get_pxweb_metadata()

cat("\ntests_get_pxweb_metadata.R : ")

api_tests_get_pxweb_metadata_baseURL <- list(
  list(baseURL = base_url("sweSCB", "v1", "sv"),
       test_dims = c(21, 4))
  )

test_that(desc="baseURL",{
  for (test in api_tests_get_pxweb_metadata_baseURL){
    ptm <- proc.time()
    expect_that({
      test_file <- get_pxweb_metadata(baseURL = test$baseURL)
    }, not(throws_error()))
    diff <- proc.time()-ptm
    Sys.sleep(max(1.1-diff[3],0))
    
    expect_that(test_file, is_a("data.frame"), info = test$baseURL)
    expect_that(dim(test_file), is_equivalent_to(test$test_dims))
  }
})
  

api_tests_get_pxweb_metadata_path <- list(
  "http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv",
  "http://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV0119/IVPKNLonAr",
  "http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNMan",
  "http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr",
  "http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ImpTotalKNMan"
)

test_that(desc="baseURL",{
  for (test in api_tests_get_pxweb_metadata_path){
    ptm <- proc.time()
    expect_that({
      test_file <- get_pxweb_metadata(path = test)
    }, not(throws_error()))
    diff <- proc.time()-ptm
    Sys.sleep(max(1.1-diff[3],0))
    
    expect_is(object = test_file$variables$variables[[1]], "list")
  }
})




# test_that(desc="get_pxweb_metadata works",{
#   testFile <- get_pxweb_metadata(baseURL = base_url("sweSCB", "v1", "sv"))
#   expect_that(testFile,is_a("data.frame"))
#   expect_that(dim(testFile),is_equivalent_to(c(21,4)))
# 
#   testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv")
#   expect_is(object=testFile$variables$variables[[1]], "list")
#   
#   testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV0119/IVPKNLonAr")
#   expect_is(object=testFile$variables$variables[[1]], "list")
#   
#   testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNMan")
#   expect_is(object=testFile$variables$variables[[1]], "list")
#   
#   testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr")
#   expect_is(object=testFile$variables$variables[[1]], "list")
#   
#   testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr")
#   expect_is(object=testFile$variables$variables[[1]], "list")
#   
#   testFile<-get_pxweb_metadata(path="http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ImpTotalKNMan")
#   expect_is(object=testFile$variables$variables[[1]], "list")
#   
# })
# cat("\n")
