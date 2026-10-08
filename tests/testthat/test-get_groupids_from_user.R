test_that("get_groupids_from_user() returns ids named by group name", {
  vcr::local_cassette("groupids_from_user")
  ids <- get_groupids_from_user(zotero_user_id = "5760254")
  expect_type(ids, "character")
  expect_gt(length(ids), 0)
  expect_false(is.null(names(ids)))
  expect_true(all(grepl("^[0-9]+$", ids)))
  expect_true(all(nzchar(names(ids))))
})
