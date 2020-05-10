# Test suits for the examples in the documentation

context("defunct")

test_that(desc="Assert defunct errors",{
  expect_error(api_catalogue(), class = "defunctError")
  expect_error(update_pxweb_apis(), class = "defunctError")
  expect_error(api_parameters(), class = "defunctError")
  expect_error(ApiData(), class = "defunctError")  
  expect_error(MakeUrl(), class = "defunctError") 
  expect_error(base_url(), class = "defunctError") 
  expect_error(get_pxweb_data(), class = "defunctError")   
  expect_error(get_pxweb_dims(), class = "defunctError") 
  expect_error(get_pxweb_levels(), class = "defunctError") 
  expect_error(get_pxweb_metadata(), class = "defunctError") 
  expect_error(pxweb_api$new(), class = "defunctError")   
  expect_error(checkForLevels(), class = "defunctError")     
})  

