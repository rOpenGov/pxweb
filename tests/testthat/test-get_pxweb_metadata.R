# Test suite for get_pxweb_metadata()

context("get_pxweb_metadata.R")


test_that(desc="baseURL 1",{
  
  skip_on_cran()
  
  suppressWarnings(
  api_tests_get_pxweb_metadata_baseURL <- list(
    list(baseURL = paste0(pxweb_api$new("api.scb.se")$base_url(language = "sv"), "/ssd"),
         test_dims = c(21, 4)),
    list(baseURL = paste0(pxweb_api$new("api.scb.se")$base_url(), "/ssd"),
         test_dims = c(17, 4))
  ))
  
  for (test in api_tests_get_pxweb_metadata_baseURL){
    expect_warning(test_file <- get_pxweb_metadata(baseURL = test$baseURL), regexp = "deprecated")
    expect_that(test_file, is_a("data.frame"), info = test$baseURL)
    expect_that(dim(test_file), is_equivalent_to(test$test_dims))
  }
})
  



test_that(desc="baseURL 2",{
  skip("Until next version")
  skip_on_cran()
  
  api_tests_get_pxweb_metadata_path <- list(
    "http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv",
    "http://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV0119/IVPKNLonAr",
    "http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNMan",
    "http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ExpTotalKNAr",
    "http://api.scb.se/OV0104/v1/doris/sv/ssd/HA/HA0201/HA0201B/ImpTotalKNMan",
    "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0401/BE0401A/BefolkprognRev2015",
    "http://api.scb.se/OV0104/v1/doris/en/ssd/UF/UF0536/Fullfoljt"
  )

  for (test in api_tests_get_pxweb_metadata_path){
    expect_warning(api_test_file <- get_pxweb_metadata(path = test), regexp = "deprecated")
    expect_is(object = api_test_file$variables$variables[[1]], "list")
  }
})

