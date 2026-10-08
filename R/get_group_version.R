#' Get the Library Version of a Zotero Group
#'
#' Returns the `Last-Modified-Version` of a Zotero group: an integer that increases
#' whenever anything in the group changes. Store it after a download and compare it
#' with the current value to decide whether a re-download is necessary.
#'
#' @param group_id The ID of the Zotero group.
#' @param api_key API key for Zotero. Only needed for private groups.
#'
#' @return The library version as an integer.
#'
#' @importFrom httr2 request req_headers req_url_query req_perform req_retry resp_header resp_status
#'
#' @md
#'
#' @examples
#' \dontrun{
#' get_group_version(2352922)
#' }
#'
#' @export
get_group_version <- function(
  group_id = 2352922,
  api_key = NULL
) {
  req <- paste0("https://api.zotero.org/groups/", group_id, "/items") |>
    httr2::request() |>
    httr2::req_retry(
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 503)
      },
      max_tries = 10
    ) |>
    httr2::req_headers("Zotero-API-Version" = 3) |>
    httr2::req_url_query("format" = "keys", "limit" = 1)

  if (!is.null(api_key)) {
    req <- httr2::req_url_query(req, "key" = api_key)
  }

  req |>
    httr2::req_perform() |>
    httr2::resp_header("Last-Modified-Version") |>
    as.integer()
}
