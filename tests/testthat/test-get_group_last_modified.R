test_that("get_group_last_modified() returns a UTC POSIXct", {
  vcr::local_cassette("group_last_modified")
  d <- get_group_last_modified(test_group_id)
  expect_s3_class(d, "POSIXct")
  expect_length(d, 1)
  expect_identical(attr(d, "tzone"), "UTC")
  expect_false(is.na(d))
  expect_lt(d, Sys.time())
})
