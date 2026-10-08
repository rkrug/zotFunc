test_that("get_group_version() returns a single positive integer", {
  vcr::local_cassette("group_version")
  v <- get_group_version(test_group_id)
  expect_type(v, "integer")
  expect_length(v, 1)
  expect_gt(v, 0)
})

test_that("get_group_version() errors for a non-existing group", {
  vcr::local_cassette("group_version_404")
  expect_error(get_group_version(999999999))
})
