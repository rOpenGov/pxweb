context("pxweb_add_call")

test_that(desc = "pxweb_add_call sleeps for the remaining rate limit window", {
  sleep_calls <- numeric()
  local_mocked_bindings(
    Sys.sleep = function(time) {
      sleep_calls <<- c(sleep_calls, time)
    },
    .package = "base"
  )

  start_time <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  px <- list(
    config = list(
      calls_per_period = 3L,
      period_in_seconds = 10L,
      max_values_to_download = 1L
    ),
    calls = list(
      time_stamps = list(start_time + 2, start_time)
    ),
    paths = list(
      rda_file_path = file.path(tempdir(), "pxweb-add-call-test.rda")
    )
  )

  px <- pxweb:::pxweb_add_call(px, time_stamp = start_time + 4)

  expect_equal(sleep_calls, 6)
  expect_length(px$calls$time_stamps, 2)
  expect_equal(px$calls$time_stamps[[1]], start_time + 4)
  expect_equal(px$calls$time_stamps[[2]], start_time + 2)
})
