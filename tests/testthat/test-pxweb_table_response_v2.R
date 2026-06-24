
test_that(desc = "PXWEB API v2 table response fixture", {
  r <- readRDS(test_path("test_data/pxweb_table_response_v2.rds"))
  x <- suppressWarnings(httr::content(r, as = "parsed"))

  expect_silent(x <- pxweb_table_response_v2(x))
  expect_s3_class(x, "pxweb_table_response_v2")

  expect_equal(x$id, "TAB5974")
  expect_true(all(c("self", "alternate", "metadata", "data") %in% vapply(x$links, function(link) link$rel, character(1))))
  expect_match(x$updated, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that(desc = "PXWEB API v2 table response rejects non-table shapes", {
  expect_error(pxweb_table_response_v2(list(id = "TAB5974")))
})
