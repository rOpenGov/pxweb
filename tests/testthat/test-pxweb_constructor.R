# Test suits for the examples in the documentation

context("pxweb")

#' url <- "https://sv.wikipedia.org/wiki/ISO_639"
#' 
#' url <- paste0(x, "?config")
#' url = "http://api.scb.se"
#' url = "http://api.scb.se/OV0104/v1/doris/sv?config"
#' url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C"

test_that(desc="Constructor works as it should with Statistics Sweden",{
  expect_silent(pxapi1 <- pxweb(url ="http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_true(file.exists(pxapi1$paths$rda_file_path))
  expect_true(length(pxapi1$calls$time_stamps) > 0)
  expect_true(is.pxweb(pxapi1))
  expect_silent(pxapi2 <- pxweb(url ="http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))  
  expect_true(length(pxapi2$calls$time_stamps) == length(pxapi2$calls$time_stamps))
  
  if(file.exists(pxapi1$paths$rda_file_path)) file.remove(pxapi1$paths$rda_file_path)
  expect_silent(pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv?config"))
  expect_silent(pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv?config")) # Cached
  
})  


test_that(desc="Constructor works for erroneous urls",{
  expect_silent(pxapi1 <- pxweb(url ="http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))

  expect_error(pxweb(url = "api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_silent(pxweb(url = "http://api.scb.se/OV0104/v1/dosasasasas"))
  expect_error(pxweb(url = "http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/"))
  expect_error(pxweb(url = "https://sv.wikipedia.org/wiki/ISO_639"))
  expect_silent(pxweb(url = "http://api.scb.se"))
  
  if(file.exists(pxapi1$paths$rda_file_path)) file.remove(pxapi1$paths$rda_file_path)
  
  expect_error(pxweb(url = "api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_error(pxweb(url = "http://api.scb.se/OV0104/v1/dosasasasas"))
  expect_error(pxweb(url = "http://api.scb.se"))
  
})  






