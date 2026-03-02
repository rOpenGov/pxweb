
test_that(desc = "pxweb API v2", {
  # Keep same style as existing live API tests.
  skip_on_cran()
  skip_if_offline()

  r <- readRDS(test_path("test_data/pxweb_table_response_v2.rds"))
  x <- suppressWarnings(httr::content(r, as = "parsed"))
  expect_silent(x <- pxweb_table_response_v2(x))
  expect_s3_class(x, "pxweb_table_response_v2")

#  pxapi_v2 <- pxweb_get(url = "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/")
})



