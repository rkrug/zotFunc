# Check if a Downloaded Zotero Group is Outdated

Compares the library version stored in `group_dir/.version` (written by
[`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md))
with the current version on the Zotero server.

## Usage

``` r
group_outdated(group_dir, group_id = NULL, api_key = NULL)
```

## Arguments

- group_dir:

  Folder of a previous
  [`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md)
  download (containing `.id`, `.name` and `.version`).

- group_id:

  Optional. If given, it must match the group stored in `group_dir/.id`.

- api_key:

  API key for Zotero. Only needed for private groups.

## Value

A named logical, named with the group id: `TRUE` if the server version
is larger than the downloaded one, `FALSE` if they are identical. An
error is raised if the server version is *smaller* than the downloaded
one, if `.id` / `.version` are missing, or if `group_id` does not match
`.id`.

## Examples

``` r
if (FALSE) { # \dontrun{
group_outdated("zotero_data")
} # }
```
