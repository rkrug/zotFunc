#' Check if a Downloaded Zotero Group is Outdated
#'
#' Compares the library version stored in `group_dir/.version` (written by [get_group()])
#' with the current version on the Zotero server.
#'
#' @param group_dir Folder of a previous [get_group()] download (containing `.id`, `.name` and `.version`).
#' @param group_id Optional. If given, it must match the group stored in `group_dir/.id`.
#' @param api_key API key for Zotero. Only needed for private groups.
#'
#' @return A named logical, named with the group id: `TRUE` if the server version is
#'   larger than the downloaded one, `FALSE` if they are identical.
#'   An error is raised if the server version is *smaller* than the downloaded one,
#'   if `.id` / `.version` are missing, or if `group_id` does not match `.id`.
#'
#' @md
#'
#' @examples
#' \dontrun{
#' group_outdated("zotero_data")
#' }
#'
#' @export
group_outdated <- function(
  group_dir,
  group_id = NULL,
  api_key = NULL
) {
  group_file <- file.path(group_dir, ".id")
  version_file <- file.path(group_dir, ".version")

  if (!file.exists(group_file) || !file.exists(version_file)) {
    stop("`", group_dir, "` has no `.id` and `.version` files - not a `get_group()` download.")
  }

  local_group <- trimws(readLines(group_file, n = 1, warn = FALSE))
  local_version <- as.integer(readLines(version_file, n = 1, warn = FALSE))

  if (!is.null(group_id) && as.character(group_id) != local_group) {
    stop(
      "`", group_dir, "` contains group ", local_group,
      " but group ", group_id, " was requested."
    )
  }

  server_version <- get_group_version(local_group, api_key = api_key)

  if (server_version < local_version) {
    stop(
      "Server version (", server_version, ") of group ", local_group,
      " is older than the downloaded version (", local_version, ")."
    )
  }

  stats::setNames(server_version > local_version, local_group)
}
