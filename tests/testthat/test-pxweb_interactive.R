# Test suits for the examples in the documentation

context("pxweb_interactive")

test_that(desc="Basic usage",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  expect_silent(pxe <- pxweb:::pxweb_explorer.character("http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_output(pxweb:::print.pxweb_explorer(pxe), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
  expect_output(pxweb:::print.pxweb_explorer(pxe), "api.scb.se")
  expect_output(pxweb:::print.pxweb_explorer(pxe), "\\[\\[Region\\]\\]")  
  
  expect_output(pxe_1 <- pxweb:::pxweb_interactive_input(pxe, test_input = "1"), "Separate multiple choices by")  
  expect_output(pxweb:::print.pxweb_explorer(pxe_1), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
  expect_output(pxweb:::print.pxweb_explorer(pxe_1), "api.scb.se")
  expect_output(pxweb:::print.pxweb_explorer(pxe_1), "\\[\\[Parti\\]\\]")  
  
  expect_output(pxe_b <- pxweb:::pxweb_interactive_input(pxe, test_input = "b"), "Separate multiple choices by")  
  expect_output(pxweb:::print.pxweb_explorer(pxe_b), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C")
  expect_output(pxweb:::print.pxweb_explorer(pxe_b), "api.scb.se")
  
  # Incorrect input
  expect_error(capture_output(pxe_error <- pxweb:::pxweb_interactive_input(pxe, test_input = "9999")), "incorrect")
  expect_error(capture_output(pxe_error <- pxweb:::pxweb_interactive_input(pxe, test_input = character(0))), "incorrect")
  
})  


test_that(desc="API catalogue usage",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  expect_silent(pxe_scb <- pxweb:::pxweb_explorer.character("api.scb.se"))
  expect_output(pxweb:::print.pxweb_explorer(pxe_scb), "v1")
  expect_output(pxe_scb_1 <- pxweb:::pxweb_interactive_input(pxe = pxe_scb, test_input = "1")) 
  expect_output(pxweb:::print.pxweb_explorer(pxe_scb_1), "en")
  expect_output(pxe_scb_1_1 <- pxweb:::pxweb_interactive_input(pxe = pxe_scb_1, test_input = "1")) 
  expect_output(pxweb:::print.pxweb_explorer(pxe_scb_1_1), "Statistics Sweden")
  expect_output(pxe_scb_1_1_b <- pxweb:::pxweb_interactive_input(pxe = pxe_scb_1_1, test_input = "b")) 
  expect_output(pxweb:::print.pxweb_explorer(pxe_scb_1_1_b), "en")
  
  
  # Check APIS  
  expect_silent(pxe_null <- pxweb:::pxweb_explorer.NULL())
  expect_output(pxweb:::print.pxweb_explorer(pxe_null), "R PXWEB API CATALOGUE")
  expect_output(pxe_1 <- pxweb:::pxweb_interactive_input(pxe = pxe_null, test_input = "1"))  
  expect_output(pxweb:::print.pxweb_explorer(pxe_1), "api.scb.se")
  expect_output(pxweb:::print.pxweb_explorer(pxe_1), "v1")
  
  expect_silent(pxe_null <- pxweb:::pxweb_explorer.NULL())
  expect_output(pxweb:::print.pxweb_explorer(pxe_null), "R PXWEB API CATALOGUE")
  expect_output(pxe_1 <- pxweb:::pxweb_interactive_input(pxe = pxe_null, test_input = "14"))  
  expect_output(pxweb:::print.pxweb_explorer(pxe_1), "px.rsv.is")

})  


test_that(desc="Select all and eliminate",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  expect_silent(pxe <- pxweb:::pxweb_explorer.character("http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_output(pxweb:::print.pxweb_explorer(pxe), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
  expect_output(pxe_star <- pxweb:::pxweb_interactive_input(pxe, test_input = "*"), "Separate multiple choices by")  
  expect_equal(pxweb:::pxe_metadata_choices(pxe_star)[[1]], 1:32)
  expect_output(pxe_star_e <- pxweb:::pxweb_interactive_input(pxe_star, test_input = "e"), "Separate multiple choices by")  
  expect_equal(pxweb:::pxe_metadata_choices(pxe_star_e)[[2]], "eliminate")
})  


test_that(desc="Select all and eliminate",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  expect_silent(pxe <- pxweb:::pxweb_explorer.character("http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "e"))  
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1:2"))  
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1"))  
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1"))  
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1")) 
  
  expect_output(dat <- pxweb:::pxe_interactive_get_data(pxe, test_input = c("n", "y", "n", "n")))
  expect_s3_class(dat, "pxweb_data")
  expect_silent(df <- pxweb:::as.data.frame.pxweb_data(dat))
  expect_equal(ncol(df), 4)
  expect_equal(nrow(df), 2)
  expect_equal(as.character(df[, 1]), c("Moderaterna", "Centerpartiet"))
  expect_output(dat <- pxweb:::pxe_interactive_get_data(pxe, c("n", "n"))) 
  expect_null(dat)
  
  expect_error(dat <- capture.output(pxweb:::pxe_interactive_get_data(pxe, test_input = "1")))
  expect_error(dat <- capture.output(pxweb:::pxe_interactive_get_data(pxe, test_input = "0")))  
  expect_error(dat <- capture.output(pxweb:::pxe_interactive_get_data(pxe, 1)))
  expect_error(dat <- capture.output(pxweb:::pxe_interactive_get_data(pxe, 0)))
  
  
  expect_output(pxweb:::pxe_print_download_code(pxe, "json"), "STORE AS JSON FILE")
  expect_output(pxweb:::pxe_print_download_code(pxe, "json"), "# Download data")
  expect_output(pxweb:::pxe_print_download_code(pxe, "json"), "# Convert to data.frame")
  
  expect_output(pxweb:::pxe_print_download_code(pxe, "r"), "# PXWEB query")
  expect_output(pxweb:::pxe_print_download_code(pxe, "r"), "pxweb_query_list")
  expect_output(pxweb:::pxe_print_download_code(pxe, "r"), "# Download data")
  expect_output(pxweb:::pxe_print_download_code(pxe, "r"), "# Convert to data.frame")
})  



test_that(desc="Stat Iceland structure",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  expect_silent(pxe <- pxweb:::pxweb_explorer.NULL())
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "10")) 
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1"))
  expect_output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1"))

  # Going back
  expect_error(capture.output(pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "b")))
  expect_output(try_error <- try(pxweb:::pxweb_interactive_input(pxe, test_input = "b"), silent = TRUE))
  expect_equal(as.character(try_error), expected = "Error : Too many incorrect choices. Aborting.\n")
})  


test_that(desc="No value bug",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/utanrikisverslun/1_voruvidskipti/02_uttollskra/UTA02801.px"
  expect_silent(pxe <- pxweb:::pxweb_explorer.character(url))
  expect_output(pxweb:::print.pxweb_explorer(pxe), regexp = "\\[\\[HS-Number\\]\\]")
})  

