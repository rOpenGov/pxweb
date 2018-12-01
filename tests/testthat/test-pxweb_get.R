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
  expect_length(pxweb_data_comments(x = px_data), 2)
  
  
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
  expect_length(pxweb_data_comments(x = px_data), 2)
})  

test_that(desc="Previous bugs",{
  # This is a bug in the previous implementation of pxweb
  url <- "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
  expect_silent(px_meta_data <- pxweb_get(url))
  expect_output(print(px_meta_data), regexp = "PXWEB LEVELS")
  
  # Missing title
  expect_silent(pxmd <- pxweb_get(url = "http://statistik.linkoping.se/PXWeb/api/v1/sv/Omsorg/Behandlingshem/ombeh01.px"))
})  



test_that(desc="Test to download json-stat objects",{
  # Test json-stat
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_single_query_test.json")
  jqf <- paste(readLines(json_query), collapse = " "); class(jqf) <- "json"
  
  jq <- gsub("json", "json-stat", jqf)
  pxq <- pxweb_query(x = jq)
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = pxq)))
  expect_s3_class(px_data, "json")

  jq <- gsub("json", "jsonstat", jqf)
  pxq <- pxweb_query(x = jq)
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = pxq)))
  expect_s3_class(px_data, "json")
  
})  


test_that(desc="Test pxweb_get_data",{
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data1 <- pxweb_get(url = url, query = json_query))
  expect_silent(px_data1_df <- as.data.frame(px_data1, column.name.type = "text", variable.value.type = "text"))
  expect_silent(px_data2 <- pxweb_get_data(url = url, query = json_query, column.name.type = "text", variable.value.type = "text"))
  expect_equal(px_data1_df, px_data2)
}) 


test_that(desc="Test http logger",{
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  expect_silent(px <- pxweb(url))
  pxweb:::pxweb_clear_cache()
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- pxweb_advanced_get(url = url, query = json_query, log_http_calls = TRUE))
  
  expect_true(file.exists(file.path(getwd(), "log_pxweb_api_http_calls.txt")))
  expect_true(file.size(file.path(getwd(), "log_pxweb_api_http_calls.txt")) > 5000)
})  

