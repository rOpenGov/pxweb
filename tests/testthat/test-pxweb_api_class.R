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
    test_api2 <- test_api$copy()
    }, 
    not(throws_error()))
  
})

