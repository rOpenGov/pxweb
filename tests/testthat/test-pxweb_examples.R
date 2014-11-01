# Test suits for the examples in the documentation

context("tests_pxweb_examples.R")

test_that(desc="Examples in get_pxweb()",{

  expect_that({ 
    url <- paste(c(base_url("api.scb.se", version = "v1", lang = "sv"),"AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
  }, not(throws_error()))
  
  expect_that({   
    metadata <- get_pxweb_metadata(url)
  }, not(throws_error()))
  
  expect_that({ 
    sink(file=tempfile())
    dims <- get_pxweb_dims(metadata)
    sink()
  }, not(throws_error()))
  
  
  expect_that({   
    test <- get_pxweb_data(metadata$URL, dims=list(
      Myndighet = "C02",
      Kon = "*",
      Heltiddeltid = "*",
      ContentsCode = "*",
      Tid = "*"))
  }, not(throws_error()))
})  





