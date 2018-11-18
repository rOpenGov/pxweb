# Test suits for the examples in the documentation

context("pxweb_interactive")

test_that(desc="Basic usage",{
  expect_silent(pxe <- pxweb_explorer("http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_output(print(pxe), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
  expect_output(print(pxe), "api.scb.se")
  expect_output(print(pxe), "\\[\\[Region\\]\\]")  
  
  expect_output(pxe_1 <- pxweb_interactive_input(pxe, test_input = "1"), "Separate multiple choices by")  
  expect_output(print(pxe_1), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
  expect_output(print(pxe_1), "api.scb.se")
  expect_output(print(pxe_1), "\\[\\[Parti\\]\\]")  
  
  expect_output(pxe_b <- pxweb_interactive_input(pxe, test_input = "b"), "Separate multiple choices by")  
  expect_output(print(pxe_b), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C")
  expect_output(print(pxe_b), "api.scb.se")
  
  # Incorrect input
  expect_error(capture_output(pxe_error <- pxweb_interactive_input(pxe, test_input = "9999")), "incorrect")
  
  # expect_error(capture_output(pxe_error <- pxweb_interactive_input(pxe, test_input = "")), "incorrect")
  
})  






