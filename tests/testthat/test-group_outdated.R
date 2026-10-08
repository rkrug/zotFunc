test_that("group_outdated() is TRUE when the server version is larger", {
  vcr::local_cassette("group_version")
  path <- fake_download(version = 1)
  res <- group_outdated(path)
  expect_identical(res, stats::setNames(TRUE, as.character(test_group_id)))
})

test_that("group_outdated() is FALSE when the versions are equal", {
  path <- fake_download(version = recorded_version())
  vcr::local_cassette("group_version")
  expect_false(unname(group_outdated(path)))
})

test_that("group_outdated() errors when the server version is older", {
  vcr::local_cassette("group_version")
  path <- fake_download(version = .Machine$integer.max)
  expect_error(group_outdated(path), "older than the downloaded version")
})

test_that("group_outdated() errors when group_id does not match .id", {
  path <- fake_download()
  expect_error(group_outdated(path, group_id = 1), "but group 1 was requested")
})

test_that("group_outdated() errors when .id or .version is missing", {
  path <- withr::local_tempdir()
  expect_error(group_outdated(path), "not a `get_group\\(\\)` download")

  path <- fake_download()
  file.remove(file.path(path, ".version"))
  expect_error(group_outdated(path), "not a `get_group\\(\\)` download")
})
