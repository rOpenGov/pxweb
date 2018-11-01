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
  
})  



