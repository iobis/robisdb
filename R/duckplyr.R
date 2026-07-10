#' Wrap a connection as a lazy duckplyr tibble (experimental)
#'
#' @description
#' An escape hatch for exploring a connection with familiar `dplyr` syntax, evaluated as
#' an *evidence-based decision*, not an oversight: `duckplyr` doesn't cope with this
#' dataset's deeply nested `interpreted`/`source`/`extensions` struct columns well enough
#' to replace the `select_db()`/`mutate_db()`/`filter_db()`/... verbs in `R/manual.R`,
#' which remain the supported query path. See that file's `@description` for the reasoning.
#'
#' This function wraps `table` (default `"obis"`) as a `duckplyr::as_duckdb_tibble()`
#' lazy tibble and, for open-data connections with `unnest_fields = TRUE` (the default),
#' flattens `interpreted`/`source`/`extensions` one level via `dd::unnest()` so their
#' fields can be referenced as plain (unprefixed) column names in subsequent `dplyr` verbs
#' -- e.g. `to_duckplyr(con) |> filter(depth > 10) |> select(scientificName, depth)`.
#'
#' `extensions` unnesting only promotes its raw URI-keyed fields (e.g.
#' `` `http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact` ``) one level; it does
#' not flatten each measurement/DNA record's `source` fields the way `occurrence_db()`'s
#' `mof`/`dna` columns do.
#'
#' **Use with care.** `duckplyr` silently materializes the *entire* table into R memory
#' when it hits an operation (or, per the reasoning above, any of this package's own
#' geometry/H3/extension SQL) it can't translate -- there is no warning by default. Narrow
#' with `filter_db()`/`select_db()` (from `R/manual.R`) *before* calling `to_duckplyr()`;
#' never call this on a fresh, unfiltered connection over the full dataset.
#'
#' @param connection a `robisdb_conn` object.
#' @param table the view/table name to wrap (default `"obis"`).
#' @param unnest_fields flatten `interpreted`/`source`/`extensions` one level via
#'   `dd::unnest()` (default `TRUE`; only applies to open-data connections).
#' @return a lazy `duckplyr` tibble, or (if construction fails) whatever `try()` returns.
#' @export
to_duckplyr <- function(connection, table = "obis", unnest_fields = TRUE) {
    require(duckplyr)
    tb <- try(dplyr::tbl(connection$connection, table) |>
                duckplyr::as_duckdb_tibble())
    if (!inherits(tb, "try-error")) {
        .s("duckplyr table created")
        ct <- connection$details$type
        if (unnest_fields & grepl("open-data", ct)) {
            if ("interpreted" %in% colnames(tb)) {
                tb <- tb |>
                    duckplyr:::mutate.duckplyr_df(dd::unnest(interpreted))
            }
            if ("source" %in% colnames(tb)) {
                tb <- tb |>
                    duckplyr:::mutate.duckplyr_df(dd::unnest(source))
            }
            if ("extensions" %in% colnames(tb)) {
                tb <- tb |>
                    duckplyr:::mutate.duckplyr_df(dd::unnest(extensions))
            }
        }
    } else {
        .a("Failed to create a duckplyr table")
    }
    return(tb)
}