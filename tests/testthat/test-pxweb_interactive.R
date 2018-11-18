# Test suits for the examples in the documentation

context("pxweb_interactive")

test_that(desc="Basic usage",{
  expect_silent(pxe <- pxweb_explorer("http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"))
  expect_output(print(pxe), "/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
  expect_output(print(pxe), "api.scb.se")
  expect_output(print(pxe), "\\[\\[Region\\]\\]")  
  
})  






