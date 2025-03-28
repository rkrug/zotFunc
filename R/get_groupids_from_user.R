#' Get all the ids from all Groups from a User
#'
#' #' Get the group ids of the Groups of the user with the zotero_user_name
#' This function requires a Zotero API key.
#' After logging into Zotero, go to the following page:
#'
#' [Home > Settings > Secutrity](https://www.zotero.org/settings/security#applications)
#'
#'   and **Create New Private Key** with the following permissions:
#'
#'   Personal Library
#'       - Allow library access
#'       - Allow group access
#'
#'   Default Group Permissions
#'       - Read Only
#'
#' @param zotero_user_name Name of the user to download the groups from.
#'   If zotero_user_id is specified, not needed. Default: "ipbes".
#' @param zotero_user_id Zotero user id to download the groups from. Default: obtained
#'   through the function [\link{id_from_name}]{@link id_from_name}.
#'   NB: In programatically usage, the id f=should be specified sxplicitely. Please see the
#'   documentation for \link{id_from_name} for details.
#' @param api_key Zotero API key - only needed for private groups. Only read access needed.
#'
#' @return Named vector with the group ids, and the names of the groups.
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
    verbose = FALSE) {
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
