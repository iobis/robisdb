#' robisdb: Fast Access to OBIS Data via DuckDB and Parquet
#'
#' @description
#' \if{html}{\figure{logo.png}{options: style='float:right' alt='robisdb logo' width='120'}}
#'
#' Query OBIS (Ocean Biodiversity Information System) open data and derived products
#' directly with DuckDB against their public Parquet snapshots, instead of paging through
#' the OBIS API. Provides drop-in equivalents of key \pkg{robis} functions
#' ([occurrence_db()], [checklist_db()], [measurements_db()]), a lightweight dplyr-style
#' query builder ([select_db()], [mutate_db()], [filter_db()], [group_by_db()],
#' [summarize_db()], [collect_db()]) for the parts of the schema those functions don't
#' cover, and helpers for spatial ([filter_spatial_db()]) and H3 hexagonal-indexing
#' ([h3_index_db()]) operations.
#'
#' @section Getting started:
#' Connect with [connect()] (or a wrapper like [connect_opendata_local()]) -- syncing a
#' local copy first with [sync_opendata()]/[sync_opendata_cli()] is much faster than
#' streaming from S3 for anything beyond a quick look.
#'
#' @section Main functions:
#' - [occurrence_db()], [checklist_db()], [measurements_db()]/[dna_db()] mirror
#'   \pkg{robis}'s functions of the same name (minus the `_db` suffix).
#' - [select_db()]/[mutate_db()]/[filter_db()]/[group_by_db()]/
#'   [summarize_db()]/[show_sql()]/[collect_db()] build a query against the `obis` view
#'   step by step; see [manual_query] for why this exists instead of a `duckplyr` pipeline.
#' - [filter_spatial_db()] and [h3_index_db()]/[h3_parent_db()]/[h3_children_db()] add
#'   spatial and H3 hexagonal-indexing operations to that same query builder.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
