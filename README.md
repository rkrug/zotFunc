# zotFunc

<!-- badges: start -->
[![R-CMD-check](https://github.com/rkrug/zotFunc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rkrug/zotFunc/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/rkrug/zotFunc/graph/badge.svg)](https://app.codecov.io/gh/rkrug/zotFunc)
[![pkgdown](https://github.com/rkrug/zotFunc/actions/workflows/pkgdown.yaml/badge.svg)](https://rkrug.github.io/zotFunc/)
<!-- badges: end -->

Download complete Zotero group libraries via the
[Zotero Web API v3](https://www.zotero.org/support/dev/web_api/v3/start),
in any of Zotero's export formats, and keep the local copies up to date by
re-downloading only groups that changed on the server.

## Installation

```r
# install.packages("pak")
pak::pak("rkrug/zotFunc")
```

## Usage

### Download a group

```r
library(zotFunc)

# Public group "Nature Futures Framework" as BibTeX
get_group(4937409, path = "nff", output_format = "bibtex")
```

The records are saved in pages of 100 (one file per page). Next to them,
`get_group()` writes four small files:

| File       | Content                                                    |
|------------|------------------------------------------------------------|
| `.id`      | the group id                                               |
| `.name`    | the group name                                             |
| `.version` | the group's library version at the start of the download   |
| `.format`  | the output format (`json` for the default JSON)             |

### Update a download

Because the group id and the format are stored in the folder, the folder is all you need:

```r
get_group(path = "nff")
#> Group 4937409 in `nff` is up to date - nothing downloaded.
```

The download is only replaced when the library version on the server is newer.
Use `force = TRUE` to download anyway, or `update = FALSE` to refuse touching an
existing download. A folder that is not a previous download is never deleted
unless you pass `overwrite = TRUE`. Giving a different `output_format` downloads the group again in
the new format. An error is raised if the folder holds a different group, or if
the server version is older than the downloaded one.

To check without downloading:

```r
group_outdated("nff")
#> 4937409
#>   FALSE
```

### Download all groups of a user

Public groups work without an API key; for private groups you need a key with
read access ([create one here](https://www.zotero.org/settings/security#applications)):

```r
get_groups_from_user(
  zotero_user_name = "ipbes",
  path = "ipbes_groups",
  output_format = "bibtex",
  api_key = Sys.getenv("ZOTERO_API_KEY")
)
```

Each group goes into its own sub-folder and is updated as described above. The
user is stored in `.user_id` and `.user_name`, so later runs only need
`get_groups_from_user(path = "ipbes_groups", api_key = ...)`.

### Other helpers

- `get_group_version()`: the current library version of a group (an integer
  that increases with every change).
- `get_group_last_modified()`: the modification time of the most recently changed item.
- `id_from_name()`: the numeric user id for a Zotero user name.
- `get_groupids_from_user()`: the ids and names of a user's groups.
