#' Get the Last Modified Date of a Zotero Group
#'
#' Queries the most recently modified item of a Zotero group and returns its
#' modification time. Only items are considered (not collections or tags).
#'
#' @param group_id The ID of the Zotero group.
#' @param api_key API key for Zotero. Only needed for private groups.
#'
#' @return The last modification time as a `POSIXct` (UTC), or `NA` if the group has no items.
#'
#' @importFrom httr2 request req_headers req_url_query req_perform req_retry resp_body_json resp_status
#'
#' @md
#'
#' @examples
#' \dontrun{
#' get_group_last_modified(2352922)
#' }
#'
#' @export
get_group_last_modified <- function(
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
    httr2::req_url_query(
      "format" = "json",
      "limit" = 1,
      "sort" = "dateModified",
      "direction" = "desc"
    )

  if (!is.null(api_key)) {
    req <- httr2::req_url_query(req, "key" = api_key)
  }

  items <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (length(items) == 0) {
    return(as.POSIXct(NA, tz = "UTC"))
  }

  as.POSIXct(
    items[[1]]$data$dateModified,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}
