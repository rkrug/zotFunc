# Get the Last Modified Date of a Zotero Group

Queries the most recently modified item of a Zotero group and returns
its modification time. Only items are considered (not collections or
tags).

## Usage

``` r
get_group_last_modified(group_id = 2352922, api_key = NULL)
```

## Arguments

- group_id:

  The ID of the Zotero group.

- api_key:

  API key for Zotero. Only needed for private groups.

## Value

The last modification time as a `POSIXct` (UTC), or `NA` if the group
has no items.

## Examples

``` r
if (FALSE) { # \dontrun{
get_group_last_modified(2352922)
} # }
```
