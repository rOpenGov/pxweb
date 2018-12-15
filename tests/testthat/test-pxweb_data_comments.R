# Test suits for the examples in the documentation

context("pxweb_data_comments")

test_that(desc="test data comment structure",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data1 <- pxweb_get(url = url, query = json_query))
  expect_silent(pxdcs <- pxweb_data_comments(x = px_data1))
  expect_output(print(pxdcs), regexp = "pxweb_data_comments\\[\\[2\\]\\]")
  expect_equal(length(pxdcs$pxweb_data_comments), 2)
  expect_silent(pxdc_df <- as.data.frame(pxdcs, stringsAsFactors = TRUE))
  expect_equal(dim(pxdc_df), c(2, 4))
  expect_equal(unname(unlist(lapply(pxdc_df, class))), c("integer", "integer", "factor", "factor"))
  expect_silent(pxdc_df <- as.data.frame(pxdcs, stringsAsFactors = FALSE))
  expect_equal(dim(pxdc_df), c(2, 4))
  expect_equal(unname(unlist(lapply(pxdc_df, class))), c("integer", "integer", "character", "character"))
  
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_variables_example.json")
  expect_silent(px_data2 <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_silent(pxdcs <- pxweb_data_comments(x = px_data2))
  expect_output(print(pxdcs), regexp = "NO PXWEB DATA COMMENTS")
  expect_equal(length(pxdcs$pxweb_data_comments), 0)
  expect_silent(pxdc_df <- as.data.frame(pxdcs, stringsAsFactors = FALSE))
  expect_equal(dim(pxdc_df), c(0, 4))
  expect_equal(colnames(pxdc_df), colnames(as.data.frame(pxweb_data_comments(x = px_data1))))
  expect_equal(unname(unlist(lapply(pxdc_df, class))), c("integer", "integer", "character", "character"))
  

})  
