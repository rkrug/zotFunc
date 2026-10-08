# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`zotFunc` is a small R package (only dependency: `httr2`) for bulk-downloading Zotero group libraries via the Zotero Web API v3 (`https://api.zotero.org`) and keeping them up to date. User docs: `README.md` (also the pkgdown home page, `_pkgdown.yml`, site at https://rkrug.github.io/zotFunc/) and two vignettes. GitHub Actions in `.github/workflows/`: R-CMD-check, test-coverage (Codecov, needs `CODECOV_TOKEN`), pkgdown (deploys to `gh-pages`).

## Commands

Standard R package workflow (run in R, or via the `r-btw` MCP tools `btw_tool_pkg_*`):

```r
devtools::document()   # regenerate man/*.Rd and NAMESPACE from roxygen (roxygen2 8.0.0, markdown = TRUE)
devtools::load_all()
devtools::check()
devtools::test(filter = "get_group")   # single test file
```

Tests use `vcr` cassettes in `tests/_vcr/` (recorded against the public group 4937409, "Nature Futures Framework"). Delete a cassette and re-run the test to re-record it. vcr replays each recorded request once per cassette insertion, so a test that needs the current version *and* calls code that requests it again uses the `recorded_version()` helper (own insertion) before inserting the cassette. `tests/testthat/test-online.R` hits the live API without cassettes and is skipped on CRAN/CI/offline.

`NAMESPACE` and `man/` are generated — edit the roxygen comments in `R/`, never the generated files. When adding an `httr2`/`utils` call, add its `@importFrom` tag (the code uses `pkg::fn` but NAMESPACE imports are still declared via roxygen).

## Architecture

- `get_groups_from_user(zotero_user_name | zotero_user_id, path, ...)` — top level: lists the user's groups with `get_groupids_from_user()` (only first 25, no paging) and calls `get_group()` for each into `path/<group name>_<id>/`. Uses `id_from_name()` (scrapes `"profileUserID":N` from the profile HTML; fragile) only when no id is given and there is no `.user_id`.
- `get_group(group_id, path, output_format, ...)` — validates the format, resolves `group_id`/`output_format` from `.id`/`.format` if not given, decides via `group_outdated()` whether to download, then pages through `/groups/<id>/items` 100 records at a time (follows the `Link: rel="next"` header's `start=`), writing one file per page into a temp dir, copies to `path`, and finally writes `.id`, `.name`, `.version`, `.format`.
- `group_outdated(group_dir)` compares `.version` with `get_group_version()` (the `Last-Modified-Version` header). `get_group_last_modified()` (newest item's `dateModified`) is a standalone helper; `get_group_name()` is internal.

Vignettes (`vignettes/zotFunc.Rmd` = quickstart / pkgdown "Get started", `vignettes/detailed-usage.Rmd`) have `eval = FALSE`; the shown output is pasted as `#>` comments from a real run, so update it by hand when behaviour or messages change.

## Gotchas

- `get_groups_from_user()` writes `.user_id` and `.user_name` into the top folder and reads it when no user is given; each group goes through `get_group()` (inside `try()`, so one failing group does not stop the rest).
- `get_group()` writes `.id`, `.name`, `.version` and `.format` (`json` = `output_format = NULL`) (library version taken *before* the download) into the output folder; `group_outdated()` reads them to decide whether to re-download. Version mismatch handling: server older or different group → error; equal → skip unless `force = TRUE` or the requested format differs from `.format`. A missing `output_format` (checked with `missing()`, since `NULL` means JSON) is read from `.format`.
- `update` only concerns previous downloads (folders with `.version`): `update = FALSE` → error. A non-empty folder without `.version` is only deleted with `overwrite = TRUE` (default `FALSE` → error); an empty folder is used as is. `get_groups_from_user()` passes `update`, `force` and `overwrite` to `get_group()`; its own `update = FALSE` errors if the top folder exists.
- The valid `output_format` list is duplicated in `get_group()` and `get_groups_from_user()`; keep both in sync.
- `api_key` in `get_groups_from_user()` defaults to `Sys.getenv("ZOTERO_API_IPBES")`.
