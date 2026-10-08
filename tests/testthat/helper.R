# Small public group used for all recorded tests (Nature Futures Framework)
test_group_id <- 4937409

# A temp folder that looks like a previous get_group() download
fake_download <- function(id = test_group_id, version = 1, format = NULL, env = parent.frame()) {
  path <- withr::local_tempfile(.local_envir = env)
  dir.create(path)
  writeLines(as.character(id), file.path(path, ".id"))
  writeLines("fake", file.path(path, ".name"))
  writeLines(as.character(version), file.path(path, ".version"))
  if (!is.null(format)) {
    writeLines(format, file.path(path, ".format"))
  }
  path
}

# Number of records in a csljson download folder
count_csljson <- function(path) {
  files <- list.files(path, pattern = "\\.json$", full.names = TRUE)
  sum(vapply(files, function(f) length(jsonlite::fromJSON(f, simplifyVector = FALSE)$items), 0L))
}

# Current server version of the test group, from its own cassette, so that a test
# can insert "group_version" again for the single request made by the code under test
# (vcr replays every recorded request only once per insertion).
recorded_version <- function() {
  vcr::local_cassette("group_version")
  get_group_version(test_group_id)
}
