# Test suits for the examples in the documentation

context("pxweb_get")

test_that(desc="Constructor works as it should with Statistics Sweden",{
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  expect_silent(px_meta_data <- pxweb_get(url))
  expect_output(print(px_meta_data), regexp = "PXWEB METADATA")
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
  expect_silent(px_levels <- pxweb_get(url))
  expect_output(print(px_levels), regexp = "PXWEB LEVELS")

  url <- "http://api.scb.se/OV0104/v1/doris/sv"
  expect_silent(px <- pxweb(url))
  expect_silent(px_levels <- pxweb_get(px))
  expect_output(print(px_levels), regexp = "PXWEB LEVELS")

  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_single_query_test.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_full_test_query.json")
  px <- pxweb(url)
  max_val <- px$config$max_values_to_download
  px$config$max_values_to_download <- 11
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = px, query = json_query, verbose = FALSE)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_output(print(px_data), regexp = "396 observations")
  
  px$config$max_values_to_download <- max_val
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = px, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_output(print(px_data), regexp = "396 observations")
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_big_query_example.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query, verbose = FALSE)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_output(print(px_data), regexp = "255200 observations")

})  



