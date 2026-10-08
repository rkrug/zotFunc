#' Get all Groups from a User
#'
#' Downloads every group of a Zotero user with [get_group()], each into the sub-folder
#' `<group name>_<group id>` of `path`. The update rules of [get_group()] apply to each group.
#' A group that fails prints its error and the next group continues.
#'
#' The user id and name are stored in `.user_id` and `.user_name` in `path`, so later updates only need `path`.
#'
#' Public groups work without an API key. For private groups, create a key with read access at
#' [Settings > Security](https://www.zotero.org/settings/security#applications)
#' (**Create New Private Key**, with *Allow library access*, *Allow group access* and
#' *Default Group Permissions: Read Only*).
#'
#' @param zotero_user_name Name of the user to download the groups from. Default: "ipbes".
#'   If not given and `path` contains a `.user_id` file (written by a previous run), the user id is read from there,
#'   so an existing download can be updated by giving only `path`.
#' @param zotero_user_id Zotero user id to download the groups from. Default: read from `path/.user_id`
#'   if present, otherwise obtained through the function `id_from_name`. Must match `path/.user_id` if both exist.
#'   In scripts, give the id explicitly: [id_from_name()] parses the profile page, which may change.
#' @param path Folder to save the groups in.
#' @param output_format The output_format of the files. See `get_group`. If not given, existing group
#'   downloads keep the format stored in their `.format` file; new groups use the default `"rdf_zotero"`.
#' @param api_key Zotero API key, only needed for private groups. Default: the environment variable `ZOTERO_API_IPBES`.
#' @param update If `FALSE`, an existing `path` is an error. Otherwise passed to `get_group`: existing group downloads
#'   are updated. Default: `TRUE`.
#' @param overwrite Passed to `get_group`: replace existing group folders that are not previous downloads. Default: `FALSE`.
#' @param force Passed to `get_group`: download even if the group is not outdated. Default: `FALSE`.
#'
#' @return `NULL`, invisibly. Called for the downloads.
#'
#' @md
#'
#' @importFrom httr2 request req_perform resp_body_json resp_status
#'
#' @export
#'

get_groups_from_user <- function(
  zotero_user_name = "ipbes",
  zotero_user_id = NULL, # "5760254",
  path = tempfile(),
  output_format = "rdf_zotero",
  api_key = Sys.getenv("ZOTERO_API_IPBES"),
  update = TRUE,
  force = FALSE,
  overwrite = FALSE
) {
  if (dir.exists(path)) {
    if (!update) {
      stop(
        "`path` ",
        path,
        " exists. Delete it or use `update = TRUE`."
      )
    }
  }

  user_id_file <- file.path(path, ".user_id")
  user_name_file <- file.path(path, ".user_name")
  read_first <- function(f) {
    if (file.exists(f)) trimws(readLines(f, n = 1, warn = FALSE))
  }
  local_user_id <- read_first(user_id_file)

  if (is.null(zotero_user_id)) {
    if (!is.null(local_user_id) && missing(zotero_user_name)) {
      zotero_user_id <- local_user_id
      zotero_user_name <- read_first(user_name_file)
    } else {
      zotero_user_id <- id_from_name(zotero_user_name)
    }
  } else if (missing(zotero_user_name)) {
    # id given explicitly: the default name does not necessarily belong to it
    zotero_user_name <- read_first(user_name_file)
  }

  if (!is.null(local_user_id) && local_user_id != as.character(zotero_user_id)) {
    stop(
      "`", path, "` contains groups of user ", local_user_id,
      " but user ", zotero_user_id, " was requested."
    )
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

  writeLines(as.character(zotero_user_id), user_id_file)
  if (!is.null(zotero_user_name)) {
    writeLines(zotero_user_name, user_name_file)
  }

  groups <- get_groupids_from_user(
    zotero_user_id = zotero_user_id,
    api_key = api_key
  )

  for (group in names(groups)) {
    try(
      {
        group_id <- groups[[group]]
        group_path <- file.path(path, paste0(group, "_", group_id))
        message("    Saving Group ", group, "(", group_id, ") to ", group_path)

        args <- list(
          group_id = group_id,
          path = group_path,
          api_key = api_key,
          update = update,
          force = force,
          overwrite = overwrite
        )
        # if no format is given, an existing group download keeps its own `.format`
        if (!missing(output_format) || !file.exists(file.path(group_path, ".format"))) {
          args["output_format"] <- list(output_format)
        }
        do.call(get_group, args)
      }
    )

    message("Downloaded group ", group_id, "!\n<<<<<<<\n")
  }
}
