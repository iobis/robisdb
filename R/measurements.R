#' Unnest an extension list-column into long format
#'
#' @description
#' Reshapes an [occurrence_db()] result that includes an `mof`/`dna` list-column (see its
#' `mof`/`dna` parameters) from one row per occurrence into one row per measurement/DNA
#' record, repeating `fields` (always including the id column) for each. Occurrences with
#' no records for that extension are dropped. Mirrors [robis::unnest_extension()]; unlike
#' that function, no column-padding step is needed here, since [occurrence_db()]'s
#' `mof`/`dna` columns already have a uniform column set per record (a fixed parquet struct
#' schema, not per-record sparse JSON).
#'
#' With `raw = TRUE`, `df` is instead treated as the raw shape returned by a manual query
#' against the `obis` table (e.g. via `select_db()`/`collect_db()`, not [occurrence_db()]):
#' the id column is `` `_id` `` rather than `id`, and both extensions live nested under a
#' single `extensions` column, keyed by URI, rather than split into
#' top-level `mof`/`dna` columns. Each entry there is a list of records carrying the raw
#' Darwin Core fields under `source`, wrapper `` `_id`/`_event_id`/`_occurrence_id`/id ``
#' fields, and `level`; the wrapper fields are dropped and `source`'s fields promoted
#' alongside `level`, matching the shape of [occurrence_db()]'s `mof`/`dna` columns.
#'
#' @param df an [occurrence_db()] result that includes `id` and `mof`/`dna` (or, with
#'   `raw = TRUE`, a raw query result including `` `_id` `` and `extensions`).
#' @param extension which extension to unnest: `"MeasurementOrFact"` or `"DNADerivedData"`.
#' @param fields columns from `df` to carry over into the long-format result (the id column
#'   is always included).
#' @param raw whether `df` is a raw manual-query result rather than an [occurrence_db()]
#'   result (default `FALSE`).
#' @return a `tibble`, one row per measurement/DNA record.
#' @export
unnest_extension_db <- function(df, extension = c("MeasurementOrFact", "DNADerivedData"),
                                 fields = "id", raw = FALSE) {
    extension <- match.arg(extension)

    if (isTRUE(raw)) {
        return(.unnest_extension_raw(df, extension, fields))
    }

    column <- if (extension == "MeasurementOrFact") "mof" else "dna"

    if (!("id" %in% names(df)) || !(column %in% names(df))) {
        .a("`df` must be an `occurrence_db()` result including `id` and `{column}`
           (pass `mof = TRUE`/`dna = TRUE` to `occurrence_db()`, or `raw = TRUE` for a
           manual-query result)")
    }

    fields <- unique(c("id", fields))
    tidyr::unnest(df[, c(fields, column)], cols = dplyr::all_of(column))
}

# Backs unnest_extension_db()'s `raw = TRUE` path: `df` is the untransformed shape of a
# manual query against `obis`, where both extensions are nested under a single `extensions`
# column keyed by URI, each entry a list of records (wrapper id fields + `source` + `level`)
# rather than the pre-flattened top-level `mof`/`dna` columns occurrence_db() produces.
.unnest_extension_raw <- function(df, extension, fields) {
    uri <- .ext_uri(extension)
    id_column <- intersect(c("id", "_id"), names(df))[1]

    if (is.na(id_column) || !("extensions" %in% names(df)) || !(uri %in% names(df$extensions))) {
        .a("`df` must include an `id`/`_id` column and an `extensions` column with a
           `{uri}` field (the raw shape returned by a manual query against `obis`)")
    }

    fields[fields == "id"] <- id_column
    fields <- unique(c(id_column, fields))

    records <- df$extensions[[uri]]
    keep <- !vapply(records, is.null, logical(1))

    out <- tibble::as_tibble(df[keep, fields, drop = FALSE])
    out$.records <- lapply(records[keep], function(r) dplyr::bind_cols(r$source, r["level"]))

    tidyr::unnest(out, cols = ".records")
}

#' Extract measurements from an `occurrence_db()` result
#'
#' @description
#' Convenience wrapper: `unnest_extension_db(df, "MeasurementOrFact", fields, raw)`. Mirrors
#' [robis::measurements()]. See [dna_db()] for the `DNADerivedData` equivalent.
#'
#' @inheritParams unnest_extension_db
#' @return a `tibble`, one row per measurement.
#' @export
measurements_db <- function(df, fields = "id", raw = FALSE) {
    unnest_extension_db(df, "MeasurementOrFact", fields, raw = raw)
}

#' Extract DNA-derived data from an `occurrence_db()` result
#'
#' @description
#' Convenience wrapper: `unnest_extension_db(df, "DNADerivedData", fields, raw)`. See
#' [measurements_db()] for the `MeasurementOrFact` equivalent.
#'
#' @inheritParams unnest_extension_db
#' @return a `tibble`, one row per DNA-derived-data record.
#' @export
dna_db <- function(df, fields = "id", raw = FALSE) {
    unnest_extension_db(df, "DNADerivedData", fields, raw = raw)
}
