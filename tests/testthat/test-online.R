# End-to-end test against the live Zotero API, without cassettes.
# Runs only locally (skipped on CRAN, CI and R-universe, and when offline).

test_that("download, up-to-date check and forced update work against the live API", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("api.zotero.org")
  vcr::local_vcr_configure(turned_off = TRUE)

  path <- file.path(withr::local_tempdir(), "nff")

  suppressMessages(get_group(test_group_id, path, output_format = "csljson"))

  total <- httr2::request("https://api.zotero.org/groups/4937409/items") |>
    httr2::req_url_query(format = "keys", limit = 1) |>
    httr2::req_perform() |>
    httr2::resp_header("Total-Results") |>
    as.integer()
  expect_identical(count_csljson(path), total)
  expect_false(unname(group_outdated(path)))

  # second run only checks the version
  expect_message(get_group(path = path, output_format = "csljson"), "up to date")

  # forced run replaces the folder
  before <- file.info(file.path(path, ".version"))$mtime
  Sys.sleep(1)
  suppressMessages(get_group(path = path, output_format = "csljson", force = TRUE))
  expect_gt(file.info(file.path(path, ".version"))$mtime, before)
})
