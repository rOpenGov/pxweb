# Test suite for utils functions

context("pxweb_api_class.R")

test_that(desc="pxweb_api_class",{  
  
  expect_warning(
    test_api <- 
      pxweb_api$new(api = "foo.bar",
                    url="http://httpbin.org/[lang]/[version]",
                    description = "test api",
                    languages = "status",
                    versions = "404",           
                    calls_per_period = 1,
                    period_in_seconds = 2, 
                    max_values_to_download = 10))
  
  expect_silent(suppressMessages(test_api$write_to_catalogue()))
  expect_true("foo.bar" %in% unlist(lapply(suppressWarnings(api_catalogue()), function(X) X$api)))

  expect_error(suppressWarnings(
    test_api2 <- 
      pxweb_api$new(api = c("foo", "bar"),
                    url="http://httpbin.org/[lang]/[version]",
                    description = "test api",
                    languages = "status",
                    versions = "404",           
                    calls_per_period = 1,
                    period_in_seconds = 2, 
                    max_values_to_download = 10)))

  expect_error(suppressWarnings(
    test_api2 <- 
      pxweb_api$new(api = "foo.bar",
                    url="http://httpbin.org/[lang]/[version]",
                    description = "test api",
                    calls_per_period = 1,
                    period_in_seconds = 2, 
                    max_values_to_download = 10)))

  expect_warning(test_api <- test_api$copy())

  expect_warning(test_api <- pxweb_api$new("api.scb.se"))

  expect_warning(test_api <- pxweb_api$new("scb"))
  
  expect_warning(
    test_api3 <- 
      pxweb_api$new())
  expect_silent(test_api3$check_input())

  expect_equal({
    test_api$base_url()
  }, "http://api.scb.se/OV0104/v1/doris/en")
  
  api_cat <- suppressWarnings(api_catalogue())
  for(api in api_cat){
    expect_silent(api$check_input())
    expect_is({api$pxweb_api_to_list()}, "list")
  }

})

