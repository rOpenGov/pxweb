# Test suits for the examples in the documentation

context("pxweb_interactive")

test_that(desc="Basic usage",{
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

  expect_silent(pxe_scb <- pxweb:::pxweb_explorer.character("api.scb.se"))
  expect_output(pxweb:::print.pxweb_explorer(pxe_scb), "v1")
  expect_output(pxe_scb_1 <- pxweb:::pxweb_interactive_input(pxe = pxe_scb, test_input = "1")) 
  expect_output(pxweb:::print.pxweb_explorer(pxe_scb_1), "en")
#  expect_output(pxe_scb_1_1 <- pxweb_interactive_input(pxe = pxe_scb, test_input = "1")) 
  
  # Check APIS  
#  expect_silent(pxe_null <- pxweb_explorer())
#  expect_output(print(pxe_null), "R PXWEB API CATALOGUE")
  
#  expect_output(pxe_1 <- pxweb_interactive_input(pxe = pxe_null, test_input = "1"))  
#  expect_output(print(pxe_1), "api.scb.se")
#  expect_output(print(pxe_1), "sv")
#  expect_output(print(pxe_1), "en")
  
  
  
})  



