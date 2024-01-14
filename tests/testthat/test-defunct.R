# Test suits for the examples in the documentation

context("defunct")

test_that(desc = "Assert defunct errors", {
  # Skip tests before 3.6 since defuncterrors came with 3.6
  rvma <- R.version$major
  rvmi <- as.numeric(R.version$minor)
  skip_if(rvma == "3" & rvmi < 6.0)

  expect_error(api_catalogue(), class = "defunctError")
  expect_error(update_pxweb_apis(), class = "defunctError")
  expect_error(api_parameters(), class = "defunctError")
  expect_error(base_url(), class = "defunctError")
  expect_error(get_pxweb_data(), class = "defunctError")
  expect_error(get_pxweb_dims(), class = "defunctError")
  expect_error(get_pxweb_levels(), class = "defunctError")
  expect_error(get_pxweb_metadata(), class = "defunctError")
  expect_error(pxweb_api$new(), class = "defunctError")
  expect_error(checkForLevels(), class = "defunctError")

})
