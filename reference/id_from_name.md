# Get Zotero User ID from Username

This function retrieves a Zotero user ID from a given username.

## Usage

``` r
id_from_name(username)
```

## Arguments

- username:

  The Zotero username. **Zotero Usernames are case sensitive!**

## Value

The Zotero user ID as a string.

## Details

**NB:The function relies on analysing the profile page. As this page
might change, it is not recommended to use this function
programmatically, but rather to retrieve the id and to use it
hardcoded.**

## Examples

``` r
if (FALSE) { # \dontrun{
id_from_name("ipbes")
# "5760254"
} # }
```
