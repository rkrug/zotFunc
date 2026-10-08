# get_groupids_from_user() is mocked to return only the small test group,
# so the downloads replay the get_group() cassettes.
one_group <- function(...) c("Nature Futures Framework" = as.character(test_group_id))

test_that("get_groups_from_user() rejects an unknown output_format", {
  expect_error(
    get_groups_from_user(zotero_user_id = "1", path = withr::local_tempdir(), output_format = "nonsense", api_key = NULL),
    "output_format must be one of"
  )
})

test_that("get_groups_from_user() errors if path exists and update = FALSE", {
  expect_error(
    get_groups_from_user(zotero_user_id = "1", path = withr::local_tempdir(), update = FALSE, api_key = NULL),
    "exists. Delete it or use `update = TRUE`"
  )
})

test_that("get_groups_from_user() errors if .user_id belongs to another user", {
  path <- withr::local_tempdir()
  writeLines("5760254", file.path(path, ".user_id"))
  expect_error(
    get_groups_from_user(zotero_user_id = "1", path = path, api_key = NULL),
    "contains groups of user 5760254 but user 1 was requested"
  )
})

test_that("get_groups_from_user() downloads each group and writes .user_id and .user_name", {
  local_mocked_bindings(get_groupids_from_user = one_group)
  vcr::local_cassette("group_download_csljson")
  path <- file.path(withr::local_tempdir(), "user")

  get_groups_from_user(
    zotero_user_name = "someone",
    zotero_user_id = "42",
    path = path,
    output_format = "csljson",
    api_key = NULL
  ) |>
    suppressMessages()

  expect_identical(readLines(file.path(path, ".user_id")), "42")
  expect_identical(readLines(file.path(path, ".user_name")), "someone")
  group_path <- file.path(path, paste0("Nature Futures Framework_", test_group_id))
  expect_identical(readLines(file.path(group_path, ".id")), as.character(test_group_id))
})

test_that("get_groups_from_user() reads the user from .user_id when only path is given", {
  seen <- NULL
  local_mocked_bindings(
    get_groupids_from_user = function(zotero_user_id, ...) {
      seen <<- zotero_user_id
      character()
    }
  )
  path <- withr::local_tempdir()
  writeLines("42", file.path(path, ".user_id"))
  writeLines("someone", file.path(path, ".user_name"))

  get_groups_from_user(path = path, api_key = NULL)

  expect_identical(seen, "42")
  expect_identical(readLines(file.path(path, ".user_name")), "someone")
})

test_that("get_groups_from_user() does not write the default name for an explicit id", {
  local_mocked_bindings(get_groupids_from_user = function(...) character())
  path <- withr::local_tempdir()

  get_groups_from_user(zotero_user_id = "42", path = path, api_key = NULL)

  expect_identical(readLines(file.path(path, ".user_id")), "42")
  expect_false(file.exists(file.path(path, ".user_name")))
})

test_that("get_groups_from_user() continues with the next group if one fails", {
  local_mocked_bindings(
    get_groupids_from_user = function(...) c(a = "1", b = "2"),
    get_group = function(group_id, ...) {
      if (group_id == "1") stop("boom")
      calls <<- c(calls, group_id)
    }
  )
  calls <- character()
  path <- withr::local_tempdir()

  expect_no_error(
    get_groups_from_user(zotero_user_id = "42", path = path, api_key = NULL) |>
      suppressMessages() |>
      capture.output(type = "message")
  )
  expect_identical(calls, "2")
})

test_that("get_groups_from_user() keeps each group's .format unless output_format is given", {
  formats <- list()
  local_mocked_bindings(
    get_groupids_from_user = function(...) c(old = "1", new = "2"),
    get_group = function(group_id, ...) {
      args <- list(...)
      formats[[group_id]] <<- if ("output_format" %in% names(args)) args$output_format else "<missing>"
    }
  )
  path <- withr::local_tempdir()
  dir.create(file.path(path, "old_1"))
  writeLines("ris", file.path(path, "old_1", ".format"))

  get_groups_from_user(zotero_user_id = "42", path = path, api_key = NULL) |>
    suppressMessages()
  expect_identical(formats, list("1" = "<missing>", "2" = "rdf_zotero"))

  formats <- list()
  get_groups_from_user(zotero_user_id = "42", path = path, output_format = "bibtex", api_key = NULL) |>
    suppressMessages()
  expect_identical(formats, list("1" = "bibtex", "2" = "bibtex"))
})

test_that("get_groups_from_user() passes overwrite, update and force to get_group()", {
  seen <- NULL
  local_mocked_bindings(
    get_groupids_from_user = function(...) c(a = "1"),
    get_group = function(...) seen <<- list(...)[c("update", "force", "overwrite")]
  )
  get_groups_from_user(
    zotero_user_id = "42", path = withr::local_tempdir(), api_key = NULL,
    update = TRUE, force = TRUE, overwrite = TRUE
  ) |>
    suppressMessages()
  expect_identical(seen, list(update = TRUE, force = TRUE, overwrite = TRUE))
})
