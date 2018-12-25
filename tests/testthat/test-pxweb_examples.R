# Test suits for the examples in the documentation

context("tests_pxweb_examples.R")

test_that(desc="Example tests",{
  
  skip("Skip temporarily (until new version)")

  expect_warning(
    url <- paste(c(pxweb_api$new("api.scb.se")$base_url(language = "sv"),"ssd","AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
  )
  
  expect_warning(
    metadata <- get_pxweb_metadata(url)
  )
  
  expect_warning(
    test <- get_pxweb_data(metadata$URL, dims=list(
      Myndighet = "C02",
      Kon = "*",
      Heltiddeltid = "*",
      ContentsCode = "*",
      Tid = "*")))
})  





