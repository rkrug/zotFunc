test_that("get_group_name() returns the group name", {
  vcr::local_cassette("group_name")
  expect_identical(get_group_name(test_group_id), "Nature Futures Framework")
})
