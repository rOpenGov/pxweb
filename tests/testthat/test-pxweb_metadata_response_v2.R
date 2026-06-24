
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
  r <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))
  x <- suppressWarnings(httr::content(r, as = "parsed"))

  expect_silent(x <- pxweb_metadata_v2(x))
  expect_s3_class(x, "pxweb_metadata")
  expect_equal(x$title, "Folkmängd efter inrikes/utrikes född, ålder och kön.  År 2025-2120")
  expect_equal(pxweb_metadata_dim(x), c(
    InrikesUtrikes = 3,
    Alder = 106,
    Kon = 2,
    ContentsCode = 1,
    Tid = 96
  ))
  expect_equal(pxweb_metadata_time(x), c(
    InrikesUtrikes = FALSE,
    Alder = FALSE,
    Kon = FALSE,
    ContentsCode = FALSE,
    Tid = TRUE
  ))
  expect_equal(x$variables[[1]]$values, c("13", "23", "83"))
  expect_equal(x$variables[[1]]$valueTexts, c("inrikes födda", "utrikes födda", "inrikes och utrikes födda"))
  expect_equal(attr(x, "pxweb_metadata_v2")$extension$px$tableid, "TAB5974")
})
