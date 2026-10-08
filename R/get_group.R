#' Get Zotero Group Data
#'
#' This function retrieves data from a Zotero group using the Zotero API.
#'
#' This function uses the Zotero API to retrieve data from a Zotero group. It downloads
#' the data in the format specified in `output_format`
#' and saves it in batches of 100 records to the folder specified by the `path` parameter.
#'
#' **NB: An existing `path` that is not a previous download (no `.version`) is only deleted and replaced
#' if `overwrite = TRUE`; otherwise an error is raised. An empty folder is used as is.**
#'
#' After the download, the group id and the library version at the start of the download are written to
#' the files `.id` and `.version` (and the group name in `.name`) in `path`. If `path` already contains such a download, [group_outdated()]
#' is used to check it: if the group differs from `group_id`, or the server version is older than the downloaded one,
#' an error is raised; if the versions are identical nothing is downloaded unless `force = TRUE`.
#'
#' The output format is stored in `.format` (`json` for `output_format = NULL`). If `output_format` is not
#' given, it is read from there. If it is given and differs from `.format`, the group is downloaded again
#' in the new format, even if it is up to date.
#'
#' For further information about the allowed formats and other details on the API see
#' [https://www.zotero.org/support/dev/web_api/v3/start](https://www.zotero.org/support/dev/web_api/v3/start).
#'
#' @param group_id The ID of the Zotero group. If `NULL`, it is read from the file `.id` in `path`
#'   (written by a previous download), so an existing download can be updated by giving only `path`.
#' @param path The path to save the retrieved data.
#' @param output_format The format of the output files. Supported are:
#'   - bibtex: BibTeX format
#'   - biblatex: BibLaTeX format
#'   - bookmarks: Firefox bookmarks in HTML format
#'   - coins: COinS format
#'   - csljson: Citation Style Language (CSL) JSON format
#'   - csv: Comma-separated values format
#'   - mods: MODS format
#'   - refer: Refer/BibIX format
#'   - rdf_bibliontology: RDF/Bibliographic Ontology format
#'   - rdf_dc: RDF/Dublin Core format
#'   - rdf_zotero: RDF/Zotero format
#'   - ris: RIS format
#'   - tei: TEI format
#'   - wikipedia: Wikipedia citation templates format
#'   - `NULL`: Zotero's own JSON (most complete, but slowest)
#'
#' See the [Zotero documentation](https://www.zotero.org/support/dev/web_api/v3/basics#item_export_formats)
#' for details of the formats.
#' If not given, it is read from `.format` in `path` (written by a previous download), otherwise `NULL`.
#'
#' @param api_key API key for Zotero. Only needed for private groups.
#' @param update If `TRUE`, an existing previous download in `path` is updated (see Details). If `FALSE`, an existing
#'   download is an error. Default: `TRUE`.
#' @param overwrite If `TRUE`, an existing non-empty `path` that is not a previous download is deleted and replaced.
#'   If `FALSE`, this is an error. Default: `FALSE`.
#' @param force If `TRUE`, download even if the downloaded version equals the server version. If `FALSE`, only download if the server version is larger. Default: `FALSE`.
#' @importFrom httr2 request req_headers req_url_query req_perform resp_headers resp_header resp_body_string req_retry resp_status
#' @importFrom utils read.csv write.table
#'
#' @return The `path` where the data is saved (also when nothing was downloaded).
#'
#' @examples
#' # Download the Zotero group with ID 2352922 (IPBES IAS Assessment Bibliography) as JSON
#' # into the folder "zotero_data"
#'
#' \dontrun{
#' get_group(2352922, "zotero_data")
#'
#' # later: update it using only the folder
#' get_group(path = "zotero_data")
#' }
#'
#' @md
#'
#' @export
get_group <- function(
  group_id = NULL,
  path = tempfile(),
  output_format = NULL,
  api_key = NULL,
  update = TRUE,
  force = FALSE,
  overwrite = FALSE
) {
  format_file <- file.path(path, ".format")
  local_format <- if (file.exists(format_file)) {
    trimws(readLines(format_file, n = 1, warn = FALSE))
  }
  if (missing(output_format) && !is.null(local_format)) {
    output_format <- if (local_format == "json") NULL else local_format
  }
  format_name <- if (is.null(output_format)) "json" else output_format

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

  if (is.null(group_id)) {
    id_file <- file.path(path, ".id")
    if (!file.exists(id_file)) {
      stop("`group_id` is not given and `", path, "` has no `.id` file to read it from.")
    }
    group_id <- trimws(readLines(id_file, n = 1, warn = FALSE))
  }


  is_download <- file.exists(file.path(path, ".version"))
  is_empty <- length(list.files(path, all.files = TRUE, no.. = TRUE)) == 0

  if (dir.exists(path) && !is_empty) {
    if (is_download) {
      if (!update) {
        stop(
          "`path` ",
          path,
          " contains a download. Delete it or use `update = TRUE`."
        )
      }
      # errors if the group differs or the server is older
      outdated <- group_outdated(path, group_id = group_id, api_key = api_key)
      # downloads without `.format` (older versions of this package) count as same format
      same_format <- is.null(local_format) || local_format == format_name
      if (!outdated && !force && same_format) {
        message("Group ", group_id, " in `", path, "` is up to date - nothing downloaded.")
        return(path)
      }
      if (!same_format) {
        message("Format changes from ", local_format, " to ", format_name, " - downloading again.")
      }
    } else if (!overwrite) {
      stop(
        "`path` ",
        path,
        " exists and is not a previous download (no `.version`). Use `overwrite = TRUE` to delete it."
      )
    }
    unlink(path, recursive = TRUE)
  }

  if (is.null(output_format)) {
    ext <- ".json"
  } else if (grepl("rdf", output_format)) {
    ext <- ".rdf"
  } else if (grepl("json", output_format)) {
    ext <- ".json"
  } else if (grepl("(?=.*bib)(?=.*tex)", output_format, perl = TRUE)) {
    ext <- ".bib"
  } else {
    ext <- paste0(".", output_format)
  }

  dir.create(
    path,
    showWarnings = FALSE,
    recursive = TRUE
  )

  api_endpoint <- "https://api.zotero.org/"

  url <- paste0(
    api_endpoint,
    "groups/",
    group_id,
    "/",
    "items"
  )

  req <- url |>
    httr2::request() |>
    httr2::req_retry(
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 503)
      },
      max_tries = 10
    )

  req <- req |>
    httr2::req_headers(
      "Zotero-API-Version" = 3
    )

  req <- req |>
    httr2::req_url_query(
      "format" = output_format,
      "limit" = 100,
      "start" = 0
    )

  if (!is.null(api_key)) {
    req <- req |>
      httr2::req_url_query(
        "key" = api_key
      )
  }

  # version before the download: changes during the download trigger a re-download next time
  version <- get_group_version(group_id, api_key = api_key)

  next_start <- 0
  total <- "?"

  tmp_path <- tempfile()
  on.exit(
    unlink(tmp_path, recursive = TRUE)
  )
  dir.create(
    tmp_path,
    showWarnings = FALSE,
    recursive = TRUE
  )

  repeat {
    message(
      "Downloading 100 records starting at record ",
      next_start,
      " from ",
      total,
      " ..."
    )
    old_start <- next_start

    req <- req |>
      httr2::req_url_query(
        "start" = next_start
      )

    resp <- req |>
      httr2::req_perform()

    total <- httr2::resp_header(resp, "Total-Results", default = "?")

    writeLines(
      httr2::resp_body_string(resp),
      con = file.path(
        tmp_path,
        paste0(output_format, "_", old_start, ext)
      )
    )

    next_start <- resp |>
      httr2::resp_headers(filter = "link") |>
      as.character() |>
      strsplit(split = "\",") |>
      unlist() |>
      grep(pattern = "\"next", value = TRUE) |>
      gsub(pattern = ".*start=([0-9]+).*", replacement = "\\1")

    if (length(next_start) == 0) {
      break()
    }
  }

  file.copy(
    from = list.files(tmp_path, full.names = TRUE),
    to = path
  )
  writeLines(as.character(group_id), file.path(path, ".id"))
  writeLines(get_group_name(group_id, api_key), file.path(path, ".name"))
  writeLines(as.character(version), file.path(path, ".version"))
  writeLines(format_name, format_file)

  return(path)
}

#' Name of a Zotero group (internal)
#' @noRd
get_group_name <- function(group_id, api_key = NULL) {
  req <- httr2::request(paste0("https://api.zotero.org/groups/", group_id)) |>
    httr2::req_headers("Zotero-API-Version" = 3)
  if (!is.null(api_key)) {
    req <- httr2::req_url_query(req, "key" = api_key)
  }
  httr2::resp_body_json(httr2::req_perform(req))$data$name
}
