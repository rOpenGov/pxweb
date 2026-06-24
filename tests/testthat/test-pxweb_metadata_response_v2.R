
test_that(desc = "PXWEB API v2 metadata fixture has expected JSON-stat2 shape", {
  r <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))
  x <- suppressWarnings(httr::content(r, as = "parsed"))

  expect_equal(x$version, "2.0")
  expect_equal(x$class, "dataset")
  expect_equal(x$extension$px$tableid, "TAB5974")
  expect_named(x, c("version", "class", "label", "source", "updated", "role", "id", "size", "dimension", "extension", "value"))
  expect_equal(unlist(x$id), names(x$dimension))
  expect_true(all(vapply(x$dimension, function(dim) all(c("label", "category", "extension") %in% names(dim)), logical(1))))
})

test_that(desc = "PXWEB API v2 metadata constructor", {
  skip_if_not(exists("pxweb_metadata_v2", mode = "function"), "pxweb_metadata_v2() is not implemented yet.")

  r <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))
  x <- suppressWarnings(httr::content(r, as = "parsed"))

  expect_silent(x <- pxweb_metadata_v2(x))
  expect_s3_class(x, "pxweb_metadata_v2")
})
