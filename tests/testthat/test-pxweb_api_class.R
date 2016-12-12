# Test suite for utils functions

context("pxweb_api_class.R")

test_that(desc="pxweb_api_class",{  
  
  expect_that({
    test_api <- 
      pxweb_api$new(api = "foo.bar",
                    url="http://httpbin.org/[lang]/[version]",
                    description = "test api",
                    languages = "status",
                    versions = "404",           
                    calls_per_period = 1,
                    period_in_seconds = 2, 
                    max_values_to_download = 10)
  }, 
  not(throws_error()))
  
  expect_that({suppressMessages(test_api$write_to_catalogue())}, 
              not(throws_error()))
  expect_true("foo.bar" %in% unlist(lapply(api_catalogue(), function(X) X$api)))

  expect_that({
    test_api2 <- 
      pxweb_api$new(api = c("foo", "bar"),
                    url="http://httpbin.org/[lang]/[version]",
                    description = "test api",
                    languages = "status",
                    versions = "404",           
                    calls_per_period = 1,
                    period_in_seconds = 2, 
                    max_values_to_download = 10)
  }, 
  throws_error())

  expect_that({
    test_api2 <- 
      pxweb_api$new(api = "foo.bar",
                    url="http://httpbin.org/[lang]/[version]",
                    description = "test api",
                    calls_per_period = 1,
                    period_in_seconds = 2, 
                    max_values_to_download = 10)
  }, 
  throws_error())  

  expect_that({
    test_api <- test_api$copy()
    }, 
    not(throws_error()))
  
  expect_that({
    test_api <- pxweb_api$new("api.scb.se")
  }, 
  not(throws_error()))
  
  expect_that({
    test_api <- pxweb_api$new("scb")
  }, 
  not(throws_error()))
  
  expect_that({
    test_api3 <- 
      pxweb_api$new()
    test_api3$check_input()
  }, 
  not(throws_error()))  
  
  expect_equal({
    test_api$base_url()
  }, "http://api.scb.se/OV0104/v1/doris/en")
  
  api_cat <- api_catalogue()
  for(api in api_cat){
    expect_that({api$check_input()}, not(throws_error()))
    expect_is({api$pxweb_api_to_list()}, "list")
  }

})

