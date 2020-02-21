# Test suits for the examples in the documentation

context("pxweb_api_catalogue")

test_that(desc="pxweb_api_catalogue",{

  expect_silent(pxac <- pxweb_api_catalogue())
  expect_output(print(pxac), regexp = "Api:")

  expect_silent(pxacgh <- pxweb:::pxweb_api_catalogue_from_github("test"))
  expect_output(print(pxac), regexp = "Api:")
      
  expect_equal(pxac[[1]], pxacgh[[1]])
  
})  

