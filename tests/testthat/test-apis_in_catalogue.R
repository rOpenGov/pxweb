# Test suite for utils functions

context("apis_in_catalogue.R")

names(api_parameters())

test_that(desc="test connections to apis in catalogue",{  
  apis <- names(api_parameters())[!names(api_parameters()) %in% "foo.bar"]
  
  for(api in apis){
    expect_true(suppressMessages(pxweb_api$new(api)$test_api()))
  }

})
