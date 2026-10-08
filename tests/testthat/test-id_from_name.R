test_that("id_from_name() extracts the numeric user id", {
  vcr::local_cassette("id_from_name")
  expect_identical(id_from_name("ipbes"), "5760254")
})

test_that("id_from_name() errors for an unknown user", {
  vcr::local_cassette("id_from_name_404")
  expect_error(
    id_from_name("this-user-does-not-exist-zotfunc"),
    "Username is invalid"
  )
})
