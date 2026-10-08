# Get all the ids from all Groups from a User

Lists the groups of a Zotero user.

## Usage

``` r
get_groupids_from_user(
  zotero_user_name = "ipbes",
  zotero_user_id = NULL,
  api_key = NULL,
  verbose = FALSE
)
```

## Arguments

- zotero_user_name:

  Name of the user to download the groups from. If zotero_user_id is
  specified, not needed. Default: "ipbes".

- zotero_user_id:

  Zotero user id to download the groups from. Default: obtained through
  [`id_from_name()`](https://rkrug.github.io/zotFunc/reference/id_from_name.md).
  In scripts, give the id explicitly:
  [`id_from_name()`](https://rkrug.github.io/zotFunc/reference/id_from_name.md)
  parses the profile page, which may change.

- api_key:

  Zotero API key, only needed for private groups.

- verbose:

  logical. If TRUE, output is verbose

## Value

Character vector of group ids, named with the group names.

## Details

**NB: Only the first 25 groups are returned (the Zotero default page
size).**

Public groups work without an API key. For private groups, create a key
with read access at [Settings \>
Security](https://www.zotero.org/settings/security#applications)
(**Create New Private Key**, with *Allow library access*, *Allow group
access* and *Default Group Permissions: Read Only*).
