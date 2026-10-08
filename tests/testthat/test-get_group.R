# ---- argument checks (no network) ----

test_that("get_group() rejects an unknown output_format without touching an existing download", {
  path <- fake_download()
  expect_error(
    get_group(test_group_id, path, output_format = "nonsense"),
    "output_format must be one of"
  )
  expect_true(file.exists(file.path(path, ".version")))
})

test_that("get_group() errors if path exists and update = FALSE", {
  path <- fake_download()
  expect_error(
    get_group(test_group_id, path, update = FALSE),
    "contains a download. Delete it or use `update = TRUE`"
  )
  expect_true(file.exists(file.path(path, ".version")))
})

test_that("get_group() uses an existing empty folder without overwrite", {
  vcr::local_cassette("group_download_csljson")
  path <- withr::local_tempdir()
  suppressMessages(get_group(test_group_id, path, output_format = "csljson"))
  expect_true(file.exists(file.path(path, ".version")))
})

test_that("get_group() errors if group_id is NULL and there is no .id", {
  path <- withr::local_tempdir()
  expect_error(get_group(path = path), "has no `.id` file")
})

# ---- version checks against an existing download ----

test_that("get_group() refuses a folder containing a different group", {
  path <- fake_download(id = 1)
  expect_error(get_group(test_group_id, path), "but group 4937409 was requested")
  expect_identical(readLines(file.path(path, ".id")), "1")
})

test_that("get_group() refuses when the server version is older than the download", {
  vcr::local_cassette("group_version")
  path <- fake_download(version = .Machine$integer.max)
  expect_error(get_group(test_group_id, path), "older than the downloaded version")
  expect_true(file.exists(file.path(path, ".name")))
})

test_that("get_group() skips an up-to-date download, reading the id from .id", {
  path <- fake_download(version = recorded_version())
  vcr::local_cassette("group_version")
  expect_message(res <- get_group(path = path), "up to date - nothing downloaded")
  expect_identical(res, path)
  expect_identical(readLines(file.path(path, ".name")), "fake")
})

test_that("get_group() reads the format from .format when output_format is not given", {
  path <- fake_download(version = recorded_version(), format = "csljson")
  vcr::local_cassette("group_version")
  expect_message(get_group(path = path), "up to date")
})

test_that("get_group() validates a format read from .format", {
  path <- fake_download(format = "nonsense")
  expect_error(get_group(path = path), "output_format must be one of")
})

# ---- full downloads ----

test_that("get_group() downloads all records and writes .id, .name and .version", {
  vcr::local_cassette("group_download_csljson")
  path <- file.path(withr::local_tempdir(), "dl")

  msgs <- capture_messages(
    res <- get_group(test_group_id, path, output_format = "csljson")
  )
  expect_identical(res, path)

  total <- httr2::request("https://api.zotero.org/groups/4937409/items") |>
    httr2::req_url_query(format = "keys", limit = 1) |>
    httr2::req_perform() |>
    httr2::resp_header("Total-Results") |>
    as.integer()

  # total is unknown before the first response
  expect_match(msgs[1], "starting at record 0 from \\? \\.\\.\\.")
  expect_match(msgs[2], paste0("starting at record 100 from ", total, " "))

  # all pages including the last one
  expect_identical(count_csljson(path), total)
  expect_length(list.files(path, pattern = "^csljson_[0-9]+\\.json$"), ceiling(total / 100))

  expect_identical(readLines(file.path(path, ".id")), as.character(test_group_id))
  expect_identical(readLines(file.path(path, ".name")), "Nature Futures Framework")
  expect_identical(readLines(file.path(path, ".format")), "csljson")
  expect_identical(
    as.integer(readLines(file.path(path, ".version"))),
    get_group_version(test_group_id)
  )
})

test_that("get_group() refuses an existing folder that is not a download", {
  path <- withr::local_tempdir()
  writeLines("junk", file.path(path, "junk.txt"))
  expect_error(
    get_group(test_group_id, path, output_format = "csljson"),
    "is not a previous download .* `overwrite = TRUE`"
  )
  expect_true(file.exists(file.path(path, "junk.txt")))

  # a partial download (.id but no .version) is not a download either
  writeLines(as.character(test_group_id), file.path(path, ".id"))
  expect_error(get_group(path = path), "is not a previous download")
})

test_that("get_group() replaces an existing folder that is not a download with overwrite = TRUE", {
  vcr::local_cassette("group_download_csljson")
  path <- withr::local_tempdir()
  writeLines("junk", file.path(path, "junk.txt"))

  get_group(test_group_id, path, output_format = "csljson", overwrite = TRUE) |>
    suppressMessages()

  expect_false(file.exists(file.path(path, "junk.txt")))
  expect_true(file.exists(file.path(path, ".version")))
})

test_that("get_group() re-downloads an up-to-date folder with force = TRUE", {
  path <- fake_download(version = recorded_version())
  vcr::local_cassette("group_download_force")

  get_group(path = path, output_format = "csljson", force = TRUE) |>
    suppressMessages()

  expect_identical(readLines(file.path(path, ".name")), "Nature Futures Framework")
  expect_gt(count_csljson(path), 0)
})

test_that("get_group() re-downloads an up-to-date folder when the format changes", {
  path <- fake_download(version = recorded_version(), format = "bibtex")
  vcr::local_cassette("group_download_format_change")

  msgs <- capture_messages(get_group(path = path, output_format = "csljson"))

  expect_match(msgs[1], "Format changes from bibtex to csljson - downloading again")
  expect_identical(readLines(file.path(path, ".format")), "csljson")
  expect_gt(count_csljson(path), 0)
})

test_that("get_group() writes json to .format for output_format = NULL", {
  local_mocked_bindings(get_group_version = function(...) 1L, get_group_name = function(...) "x")
  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(headers = list(Link = ""), body = charToRaw("[]"))
    },
    .package = "httr2"
  )
  path <- file.path(withr::local_tempdir(), "dl")
  suppressMessages(get_group(test_group_id, path))
  expect_identical(readLines(file.path(path, ".format")), "json")
  expect_length(list.files(path, pattern = "^_0\\.json$"), 1)
})
