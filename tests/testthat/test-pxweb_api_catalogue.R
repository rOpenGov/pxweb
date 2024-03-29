# Test suits for the examples in the documentation

context("pxweb_api_catalogue")

test_that(desc = "pxweb_api_catalogue", {
  # Problems with testing ISO8859 on CRAN
  # https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-debian-clang/pxweb-00check.html
  # Waiting to add test suites for different charsets:
  # https://github.com/r-lib/actions/issues/609
  skip_on_cran()

  # Recapture calls (in interactive mode)
  capture_calls <- FALSE

  with_mock_api({
    if(capture_calls) start_capturing()
    expect_silent(pxac <- pxweb_api_catalogue())
    expect_output(print(pxac), regexp = "Api:")
    expect_silent(pxacgh <- suppressMessages(
      pxweb:::pxweb_api_catalogue_from_github("master")))
    expect_output(print(pxac), regexp = "Api:")

    expect_equal(pxac[[2]], pxacgh[[2]])
    if(capture_calls) stop_capturing()
  })
})
