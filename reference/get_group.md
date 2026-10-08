# Get Zotero Group Data

This function retrieves data from a Zotero group using the Zotero API.

## Usage

``` r
get_group(
  group_id = NULL,
  path = tempfile(),
  output_format = NULL,
  api_key = NULL,
  update = TRUE,
  force = FALSE,
  overwrite = FALSE
)
```

## Arguments

- group_id:

  The ID of the Zotero group. If `NULL`, it is read from the file `.id`
  in `path` (written by a previous download), so an existing download
  can be updated by giving only `path`.

- path:

  The path to save the retrieved data.

- output_format:

  The format of the output files. Supported are:

  - bibtex: BibTeX format

  - biblatex: BibLaTeX format

  - bookmarks: Firefox bookmarks in HTML format

  - coins: COinS format

  - csljson: Citation Style Language (CSL) JSON format

  - csv: Comma-separated values format

  - mods: MODS format

  - refer: Refer/BibIX format

  - rdf_bibliontology: RDF/Bibliographic Ontology format

  - rdf_dc: RDF/Dublin Core format

  - rdf_zotero: RDF/Zotero format

  - ris: RIS format

  - tei: TEI format

  - wikipedia: Wikipedia citation templates format

  - `NULL`: Zotero's own JSON (most complete, but slowest)

  See the [Zotero
  documentation](https://www.zotero.org/support/dev/web_api/v3/basics#item_export_formats)
  for details of the formats. If not given, it is read from `.format` in
  `path` (written by a previous download), otherwise `NULL`.

- api_key:

  API key for Zotero. Only needed for private groups.

- update:

  If `TRUE`, an existing previous download in `path` is updated (see
  Details). If `FALSE`, an existing download is an error. Default:
  `TRUE`.

- force:

  If `TRUE`, download even if the downloaded version equals the server
  version. If `FALSE`, only download if the server version is larger.
  Default: `FALSE`.

- overwrite:

  If `TRUE`, an existing non-empty `path` that is not a previous
  download is deleted and replaced. If `FALSE`, this is an error.
  Default: `FALSE`.

## Value

The `path` where the data is saved (also when nothing was downloaded).

## Details

This function uses the Zotero API to retrieve data from a Zotero group.
It downloads the data in the format specified in `output_format` and
saves it in batches of 100 records to the folder specified by the `path`
parameter.

**NB: An existing `path` that is not a previous download (no `.version`)
is only deleted and replaced if `overwrite = TRUE`; otherwise an error
is raised. An empty folder is used as is.**

After the download, the group id and the library version at the start of
the download are written to the files `.id` and `.version` (and the
group name in `.name`) in `path`. If `path` already contains such a
download,
[`group_outdated()`](https://rkrug.github.io/zotFunc/reference/group_outdated.md)
is used to check it: if the group differs from `group_id`, or the server
version is older than the downloaded one, an error is raised; if the
versions are identical nothing is downloaded unless `force = TRUE`.

The output format is stored in `.format` (`json` for
`output_format = NULL`). If `output_format` is not given, it is read
from there. If it is given and differs from `.format`, the group is
downloaded again in the new format, even if it is up to date.

For further information about the allowed formats and other details on
the API see <https://www.zotero.org/support/dev/web_api/v3/start>.

## Examples

``` r
# Download the Zotero group with ID 2352922 (IPBES IAS Assessment Bibliography) as JSON
# into the folder "zotero_data"

if (FALSE) { # \dontrun{
get_group(2352922, "zotero_data")

# later: update it using only the folder
get_group(path = "zotero_data")
} # }
```
