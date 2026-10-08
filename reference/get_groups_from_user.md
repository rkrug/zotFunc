# Get all Groups from a User

Downloads every group of a Zotero user with
[`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md),
each into the sub-folder `<group name>_<group id>` of `path`. The update
rules of
[`get_group()`](https://rkrug.github.io/zotFunc/reference/get_group.md)
apply to each group. A group that fails prints its error and the next
group continues.

## Usage

``` r
get_groups_from_user(
  zotero_user_name = "ipbes",
  zotero_user_id = NULL,
  path = tempfile(),
  output_format = "rdf_zotero",
  api_key = Sys.getenv("ZOTERO_API_IPBES"),
  update = TRUE,
  force = FALSE,
  overwrite = FALSE
)
```

## Arguments

- zotero_user_name:

  Name of the user to download the groups from. Default: "ipbes". If not
  given and `path` contains a `.user_id` file (written by a previous
  run), the user id is read from there, so an existing download can be
  updated by giving only `path`.

- zotero_user_id:

  Zotero user id to download the groups from. Default: read from
  `path/.user_id` if present, otherwise obtained through the function
  `id_from_name`. Must match `path/.user_id` if both exist. In scripts,
  give the id explicitly:
  [`id_from_name()`](https://rkrug.github.io/zotFunc/reference/id_from_name.md)
  parses the profile page, which may change.

- path:

  Folder to save the groups in.

- output_format:

  The output_format of the files. See `get_group`. If not given,
  existing group downloads keep the format stored in their `.format`
  file; new groups use the default `"rdf_zotero"`.

- api_key:

  Zotero API key, only needed for private groups. Default: the
  environment variable `ZOTERO_API_IPBES`.

- update:

  If `FALSE`, an existing `path` is an error. Otherwise passed to
  `get_group`: existing group downloads are updated. Default: `TRUE`.

- force:

  Passed to `get_group`: download even if the group is not outdated.
  Default: `FALSE`.

- overwrite:

  Passed to `get_group`: replace existing group folders that are not
  previous downloads. Default: `FALSE`.

## Value

`NULL`, invisibly. Called for the downloads.

## Details

The user id and name are stored in `.user_id` and `.user_name` in
`path`, so later updates only need `path`.

Public groups work without an API key. For private groups, create a key
with read access at [Settings \>
Security](https://www.zotero.org/settings/security#applications)
(**Create New Private Key**, with *Allow library access*, *Allow group
access* and *Default Group Permissions: Read Only*).
