# Get the Library Version of a Zotero Group

Returns the `Last-Modified-Version` of a Zotero group: an integer that
increases whenever anything in the group changes. Store it after a
download and compare it with the current value to decide whether a
re-download is necessary.

## Usage

``` r
get_group_version(group_id = 2352922, api_key = NULL)
```

## Arguments

- group_id:

  The ID of the Zotero group.

- api_key:

  API key for Zotero. Only needed for private groups.

## Value

The library version as an integer.

## Examples

``` r
if (FALSE) { # \dontrun{
get_group_version(2352922)
} # }
```
