# Test suits for the examples in the documentation

context("urls")

test_that(desc="Check package URLs",{
  skip_on_cran()
  urls_checked <- urlchecker::url_check()
  # urlchecker::url_update() - update URLS
  expect_length(urls_checked$URL, 0L)
  
})  

