# Test suits for the examples in the documentation

context("pxweb_get")

test_that(desc="Constructor works as it should with Statistics Sweden",{
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  px_meta_data <- pxweb_get(url)
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
  px_levels <- pxweb_get(url)

  url <- "http://api.scb.se/OV0104/v1/doris/sv"
  px_levels <- pxweb_get(url)

  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  px_data <- pxweb_get(url = url, query = json_query)
  
})  


test_that(desc="Constructor works for erroneous urls",{
  expect_silent(pxapi1 <- pxweb(url ="http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))

  expect_error(pxweb(url = "api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_silent(pxweb(url = "http://api.scb.se/OV0104/v1/dosasasasas"))
  expect_error(pxweb(url = "http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/"))
  expect_error(pxweb(url = "https://sv.wikipedia.org/wiki/ISO_639"))
  expect_silent(pxweb(url = "http://api.scb.se"))
  
  expect_silent(pxweb:::pxweb_clear_cache(pxapi1))
  
  expect_error(pxweb(url = "api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_error(pxweb(url = "http://api.scb.se/OV0104/v1/dosasasasas"))
  expect_error(pxweb(url = "http://api.scb.se"))
  
})  


test_that(desc="Cache cleaner and print",{

  expect_silent(pxapi1 <- pxweb(url ="http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_silent(pxapi2 <- pxweb("http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/statfin_tyonv_pxt_001.px"))
  expect_true(file.exists(pxapi1$paths$rda_file_path))
  expect_silent(pxweb:::pxweb_clear_cache())
  expect_false(file.exists(pxapi1$paths$rda_file_path))
  
  expect_output(print(pxapi1), "PXWEB API")
})  





