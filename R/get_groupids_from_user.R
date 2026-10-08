#' Get all the ids from all Groups from a User
#'
#' Lists the groups of a Zotero user.
#'
#' **NB: Only the first 25 groups are returned (the Zotero default page size).**
#'
#' Public groups work without an API key. For private groups, create a key with read access at
#' [Settings > Security](https://www.zotero.org/settings/security#applications)
#' (**Create New Private Key**, with *Allow library access*, *Allow group access* and
#' *Default Group Permissions: Read Only*).
#'
#' @param zotero_user_name Name of the user to download the groups from.
#'   If zotero_user_id is specified, not needed. Default: "ipbes".
#' @param zotero_user_id Zotero user id to download the groups from. Default: obtained
#'   through [id_from_name()]. In scripts, give the id explicitly: [id_from_name()] parses the
#'   profile page, which may change.
#' @param api_key Zotero API key, only needed for private groups.
#' @param verbose logical. If TRUE, output is verbose
#'
#' @return Character vector of group ids, named with the group names.
#' @md
#'
#' @importFrom httr2 request req_perform resp_body_json resp_status
#'
#'
#' @md
#'
#' @export
#'

get_groupids_from_user <- function(
  zotero_user_name = "ipbes",
  zotero_user_id = NULL, # "5760254",
  api_key = NULL, # Sys.getenv("ZOTERO_API_IPBES"),
  verbose = FALSE
) {
  if (is.null(zotero_user_id)) {
    if (verbose) {
      message("Get user id from user name ...")
    }
    zotero_user_id <- id_from_name(zotero_user_name)
  }

  if (verbose) {
    message("Get group ids from user id ...")
  }
  ## extract ids from zotero_user_id
  api_url <- paste0("https://api.zotero.org/users/", zotero_user_id, "/groups")

  # Create and perform the request
  req <- httr2::request(api_url)

  if (!is.null(api_key)) {
    req <- req |>
      httr2::req_url_query(
        "key" = api_key
      )
  }
  groups <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    sapply(
      FUN = function(group) {
        c(id = group$id, name = group$data$name)
      }
    )
  ##

  result <- groups["id", ]
  names(result) <- groups["name", ]

  return(result)
}
