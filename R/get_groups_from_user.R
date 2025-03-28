#' Get all Groups from a User
#'
#' Download of the Groups of the user with the zotero_user_id.
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
#' @param zotero_user_name Name of the user to download the groups from. Default: "ipbes".
#' @param zotero_user_id Zotero user id to download the groups from. Default: obtained
#'   through the function [\link{id_from_name}]{@link id_from_name}.
#'   NB: In programatically usage, the id f=should be specified sxplicitely. Please see the
#'   documentation for \link{id_from_name} for details.
#' @param output_format The output_format of the files. See \code{\link{zotero_get_group}}
#' @param api_key Zotero API key - only needed for private groups. Only read access needed.
#'
#' @md
#'
#' @importFrom httr2 request req_perform resp_body_json resp_status
#'
#' @md
#'
#' @export
#'

get_groups_from_user <- function(
    zotero_user_name = "ipbes",
    zotero_user_id = NULL, # "5760254",
    path = tempfile(),
    output_format = "zotero_rdf",
    api_key = Sys.getenv("ZOTERO_API_IPBES") #
    ) {
  if (dir.exists(path)) {
    stop("`path` ", path, " exists and will not be overwritten. Delete it and try again.")
  }

  if (is.null(zotero_user_id)) {
    zotero_user_id <- id_from_name(zotero_user_name)
  }

  zotero_export_formats <- c(
    "bibtex",
    "biblatex",
    "bookmarks",
    "coins",
    "csljson",
    "csv",
    "mods",
    "refer",
    "rdf_bibliontology",
    "rdf_dc",
    "rdf_zotero",
    "ris",
    "tei",
    "wikipedia"
  )

  if (!is.null(output_format)) {
    if (!(output_format %in% zotero_export_formats)) {
      stop(
        "output_format must be one of the supported Zotero export formats as defined in\n",
        "     https://www.zotero.org/support/dev/web_api/v3/basics#item_export_formats\n",
        "   or `NULL for raw JSON (really slow!!!)"
      )
    }
  }

  dir.create(
    path, 
    showWarnings = FALSE, 
    recursive = TRUE
  )

  groups <- get_groupids_from_user(
    zotero_user_id = zotero_user_id,
    api_key = api_key
  )

  for (group_id in groups) {
    try({
      path <- file.path(path, paste0(group_name, "_", group_id))
      message("    Saving Group ", group_name, "(", group_id, ") to ", path)

      zotero_get_group(
        group_id = group_id,
        path = path,
        output_format = output_format,
        api_key = api_key
      )
    })

    message("Downloaded group ", group_id, "!\n<<<<<<<\n")
  }
}
