# Detailed usage

``` r

library(zotFunc)
```

This vignette covers everything
[`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md)
does when a folder already exists, downloading all groups of a user, and
the helper functions. For a first download, see
[`vignette("zotFunc")`](https://rkrug.github.io/zotFunc/articles/zotFunc.md).

## Output formats

`output_format` takes any [Zotero export
format](https://www.zotero.org/support/dev/web_api/v3/basics#item_export_formats):
`bibtex`, `biblatex`, `bookmarks`, `coins`, `csljson`, `csv`, `mods`,
`refer`, `rdf_bibliontology`, `rdf_dc`, `rdf_zotero`, `ris`, `tei` or
`wikipedia`. `NULL`, the default for a new download, gives Zotero’s own
JSON. This is the most complete format but also the slowest.

Each page of 100 records is saved as
`<format>_<first record>.<extension>`, for example `bibtex_300.bib` or
`csljson_0.json`.

## The files describing a download

| File | Content |
|----|----|
| `.id` | the group id |
| `.name` | the group name |
| `.version` | the group’s library version, read *before* the download started |
| `.format` | the output format (`json` for `output_format = NULL`) |

The version is read before the download, not after it. If someone edits
the group while a long download runs, the next update sees a newer
version and downloads again, instead of missing that edit.

## Updating an existing folder

When `path` already exists,
[`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md)
decides what to do. A *download* is a folder containing `.version`,
written at the end of a successful
[`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md).

| Situation | Result |
|----|----|
| download, `update = FALSE` | error, folder untouched |
| folder holds a different group than `group_id` | error, folder untouched |
| server version older than `.version` | error, folder untouched |
| server version newer than `.version` | folder replaced by a new download |
| versions equal, same format | nothing downloaded |
| versions equal, `force = TRUE` | folder replaced by a new download |
| versions equal, different `output_format` requested | folder replaced by a new download |
| not a download (no `.version`), `overwrite = FALSE` | error, folder untouched |
| not a download, `overwrite = TRUE` | **folder deleted**, group downloaded |
| empty folder | group downloaded into it |

If `group_id` or `output_format` are not given, they are read from `.id`
and `.format`.

``` r

# download again even though nothing changed
get_group(path = "nff", force = TRUE)

# switch the folder to CSL JSON
get_group(path = "nff", output_format = "csljson")
#> Format changes from bibtex to csljson - downloading again.
#> ...

# never touch an existing download
get_group(path = "nff", update = FALSE)
#> Error in get_group(path = "nff", update = FALSE) :
#>   `path` nff contains a download. Delete it or use `update = TRUE`.

# a folder that is not a download is protected
get_group(4937409, path = "my_documents")
#> Error in get_group(4937409, path = "my_documents") :
#>   `path` my_documents exists and is not a previous download (no `.version`). Use `overwrite = TRUE` to delete it.

# the folder belongs to another group
get_group(1, path = "nff")
#> Error in group_outdated(path, group_id = group_id, api_key = api_key) :
#>   `nff` contains group 4937409 but group 1 was requested.
```

## Checking without downloading

[`group_outdated()`](https://rkrug.github.io/zotFunc/reference/group_outdated.md)
compares a folder with the server. It returns a logical named with the
group id: `TRUE` if the server has a newer version.

``` r

group_outdated("nff")
#> 4937409
#>   FALSE
```

It raises an error if the server version is older than the downloaded
one, which should not happen with a real group. Here `.version` was set
to 999999999 by hand:

``` r

group_outdated("nff")
#> Error in group_outdated("nff") :
#>   Server version (2053) of group 4937409 is older than the downloaded version (999999999).
```

For a quick overview of several downloads:

``` r

dirs <- list.dirs("ipbes_groups", recursive = FALSE)
unlist(lapply(dirs, group_outdated))
```

## All groups of a user

[`get_groups_from_user()`](https://rkrug.github.io/zotFunc/reference/get_groups_from_user.md)
downloads every group of a Zotero user into sub-folders named
`<group name>_<group id>`, and runs
[`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md)
for each, so all the update rules above apply per group.

``` r

get_groups_from_user(
  zotero_user_name = "ipbes",
  path = "ipbes_groups",
  output_format = "bibtex",
  api_key = Sys.getenv("ZOTERO_API_KEY")
)
```

Public groups work without an API key; private groups need a key with
read access, created at
[zotero.org/settings/security](https://www.zotero.org/settings/security#applications).
Note that `api_key` defaults to the environment variable
`ZOTERO_API_IPBES`.

The user is stored in `.user_id` and `.user_name` in `path`, so a later
update only needs the folder:

``` r

get_groups_from_user(path = "ipbes_groups", api_key = Sys.getenv("ZOTERO_API_KEY"))
```

Without `output_format`, existing groups keep the format in their
`.format` and new groups use `"rdf_zotero"`. A group that fails (for
example a private group without a key) prints its error and the next
group continues.

## Helper functions

### Library version

The library version is an integer that Zotero increases with every
change to the group: items, collections, saved searches and deletions.
It is what
[`group_outdated()`](https://rkrug.github.io/zotFunc/reference/group_outdated.md)
compares.

``` r

get_group_version(4937409)
#> [1] 2053
```

### Last modification time

The modification time of the most recently changed item. Unlike the
version, it ignores changes to collections only.

``` r

get_group_last_modified(4937409)
#> [1] "2026-09-30 07:14:51 UTC"
```

### User id from user name

Most functions need the numeric user id.
[`id_from_name()`](https://rkrug.github.io/zotFunc/reference/id_from_name.md)
reads it from the public profile page:

``` r

id_from_name("ipbes")
#> [1] "5760254"
```

It parses the HTML of the profile page, which Zotero can change at any
time. In scripts, look the id up once and use it directly.

### Groups of a user

``` r

ids <- get_groupids_from_user(zotero_user_id = "5760254")
head(ids, 3)
#> IPBES NXS IPBES TCA IPBES BBA
#> "4596166" "4589462" "5081872"
```

This returns at most 25 groups, the Zotero default page size.
