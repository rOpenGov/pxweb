# Test suits for the examples in the documentation

context("tests_pxweb_examples.R")

test_that(desc="Example tests",{
  
  skip("Skip temporarily (until new version)")

  expect_that({ 
    url <- paste(c(pxweb_api$new("api.scb.se")$base_url(language = "sv"),"ssd","AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
  }, not(throws_error()))
  
  expect_that({   
    metadata <- get_pxweb_metadata(url)
  }, not(throws_error()))
  
  expect_that({ 
    sink(file=tempfile())
    dims <- suppressMessages(get_pxweb_dims(metadata))
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





