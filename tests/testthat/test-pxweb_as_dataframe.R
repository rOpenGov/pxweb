# Test suits for the examples in the documentation

context("pxweb conversions")

test_that(desc="Converting pxweb data to matrices and data.frames",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  # Move to 
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_variables_example.json")
  # save(px_data, file = "px_data_example.rda")
  # load("px_data_example.rda")
  expect_silent(pxq <- pxweb_query(json_query))
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))

  expect_silent(df <- as.data.frame(px_data, stringsAsFactors = TRUE, column.name.type = "text", variable.value.type = "text"))
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), expected = c(12,3))
  expect_true(any(unlist(lapply(df, class)) == "factor"))
  expect_equal(colnames(df), expected = c("civilstånd", "år", "Folkmängd"))

  expect_silent(df <- as.data.frame(px_data, stringsAsFactors = FALSE, column.name.type = "text", variable.value.type = "text"))
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), expected = c(12,3))
  expect_false(any(unlist(lapply(df, class)) == "factor"))
  expect_equal(colnames(df), expected = c("civilstånd", "år", "Folkmängd"))
  
  expect_silent(df <- as.data.frame(x = px_data, stringsAsFactors = FALSE, column.name.type = "code", variable.value.type = "text"))
  expect_s3_class(df, "data.frame")
  expect_equal(colnames(df), expected = c("Civilstand", "Tid", "BE0101N1"))
  expect_true(all(df$Civilstand %in% c("ogifta", "gifta", "skilda", "änkor/änklingar")))
  
  expect_silent(df <- as.data.frame(px_data, stringsAsFactors = FALSE, column.name.type = "code", variable.value.type = "code", row.names = as.character(1001:1012)))
  expect_s3_class(df, "data.frame")
  expect_true(all(df$Civilstand %in% c("OG", "G", "SK", "ÄNKL")))
  expect_equal(rownames(df), as.character(1001:1012))
  
  # Test as.matrix
  expect_silent(mat <- as.matrix.pxweb_data(x = px_data, column.name.type = "text", variable.value.type = "text"))
  expect_true(is.matrix(mat))
  expect_true(is.character(mat))
  expect_equal(dim(mat), expected = c(12,3))
  expect_equal(colnames(mat), expected = c("civilstånd", "år", "Folkmängd"))
  expect_true(all(mat[,1] %in% c("ogifta", "gifta", "skilda", "änkor/änklingar")))
  
  expect_silent(mat <- as.matrix(px_data, column.name.type = "code", variable.value.type = "code"))
  expect_true(is.matrix(mat))
  expect_true(is.character(mat))
  expect_equal(dim(mat), expected = c(12,3))
  expect_equal(colnames(mat), expected = c("Civilstand", "Tid", "BE0101N1"))
  expect_true(all(mat[,1] %in% c("OG", "G", "SK", "ÄNKL")))

})  



