
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

test_that(desc = "PXWEB API v2 metadata is parsed through pxweb_parse_response", {
  r <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))

  expect_silent(x <- pxweb_parse_response(r))
  expect_s3_class(x, "pxweb_metadata")
  expect_equal(pxweb_metadata_dim(x)[["Tid"]], 96)
})

test_that(desc = "PXWEB API v2 metadata keeps raw metadata attribute", {
  r <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))
  raw <- suppressWarnings(httr::content(r, as = "parsed"))

  x <- pxweb_metadata_v2(raw)

  expect_identical(attr(x, "pxweb_metadata_v2")$id, raw$id)
  expect_identical(attr(x, "pxweb_metadata_v2")$dimension$Tid$category$index, raw$dimension$Tid$category$index)
})

test_that(desc = "PXWEB API v2 metadata values are ordered by category index", {
  x <- list(
    version = "2.0",
    class = "dataset",
    label = "Test table",
    id = list("Region"),
    dimension = list(
      Region = list(
        label = "region",
        category = list(
          index = list(B = 1, A = 0, C = 2),
          label = list(B = "Beta", A = "Alpha", C = "Gamma")
        ),
        extension = list(elimination = TRUE)
      )
    ),
    value = list()
  )

  x <- pxweb_metadata_v2(x)

  expect_equal(x$variables[[1]]$values, c("A", "B", "C"))
  expect_equal(x$variables[[1]]$valueTexts, c("Alpha", "Beta", "Gamma"))
})

test_that(desc = "PXWEB API v2 metadata value texts fall back to value codes", {
  x <- list(
    version = "2.0",
    class = "dataset",
    label = "Test table",
    id = list("Region"),
    dimension = list(
      Region = list(
        label = "region",
        category = list(
          index = list(A = 0, B = 1),
          label = list(A = "Alpha")
        ),
        extension = list(elimination = FALSE)
      )
    ),
    value = list()
  )

  x <- pxweb_metadata_v2(x)

  expect_equal(x$variables[[1]]$values, c("A", "B"))
  expect_equal(x$variables[[1]]$valueTexts, c("Alpha", "B"))
})

test_that(desc = "PXWEB API v2 metadata without time role sets time flags to false", {
  x <- list(
    version = "2.0",
    class = "dataset",
    label = "Test table",
    id = list("Region", "Tid"),
    dimension = list(
      Region = list(
        label = "region",
        category = list(
          index = list(A = 0),
          label = list(A = "Alpha")
        ),
        extension = list(elimination = TRUE)
      ),
      Tid = list(
        label = "year",
        category = list(
          index = list("2025" = 0),
          label = list("2025" = "2025")
        ),
        extension = list(elimination = FALSE)
      )
    ),
    value = list()
  )

  x <- pxweb_metadata_v2(x)

  expect_equal(pxweb_metadata_time(x), c(Region = FALSE, Tid = FALSE))
})

test_that(desc = "PXWEB API v2 data response is not parsed as metadata", {
  x <- list(
    version = "2.0",
    class = "dataset",
    label = "Test table",
    id = list("Region"),
    dimension = list(
      Region = list(
        label = "region",
        category = list(
          index = list(A = 0),
          label = list(A = "Alpha")
        ),
        extension = list(elimination = FALSE)
      )
    ),
    value = list(42)
  )

  expect_error(pxweb_metadata_v2(x))
})
