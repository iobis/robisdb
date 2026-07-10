#' Unnest an extension list-column into long format
#'
#' @description
#' Reshapes an [occurrence_db()] result that includes an `mof`/`dna` list-column (see its
#' `mof`/`dna` parameters) from one row per occurrence into one row per measurement/DNA
#' record, repeating `fields` (always including `id`) for each. Occurrences with no records
#' for that extension are dropped. Mirrors [robis::unnest_extension()]; unlike that
#' function, no column-padding step is needed here, since [occurrence_db()]'s `mof`/`dna`
#' columns already have a uniform column set per record (a fixed parquet struct schema,
#' not per-record sparse JSON).
#'
#' @param df an [occurrence_db()] result that includes `id` and `mof`/`dna`.
#' @param extension which extension to unnest: `"MeasurementOrFact"` or `"DNADerivedData"`.
#' @param fields columns from `df` to carry over into the long-format result (`"id"` is
#'   always included).
#' @return a `tibble`, one row per measurement/DNA record.
#' @export
unnest_extension_db <- function(df, extension = c("MeasurementOrFact", "DNADerivedData"), fields = "id") {
    extension <- match.arg(extension)
    column <- if (extension == "MeasurementOrFact") "mof" else "dna"

    if (!("id" %in% names(df)) || !(column %in% names(df))) {
        .a("`df` must be an `occurrence_db()` result including `id` and `{column}`
           (pass `mof = TRUE`/`dna = TRUE` to `occurrence_db()`)")
    }

    fields <- unique(c("id", fields))
    tidyr::unnest(df[, c(fields, column)], cols = dplyr::all_of(column))
}

#' Extract measurements from an `occurrence_db()` result
#'
#' @description
#' Convenience wrapper: `unnest_extension_db(df, "MeasurementOrFact", fields)`. Mirrors
#' [robis::measurements()]. See [dna_db()] for the `DNADerivedData` equivalent.
#'
#' @inheritParams unnest_extension_db
#' @return a `tibble`, one row per measurement.
#' @export
measurements_db <- function(df, fields = "id") {
    unnest_extension_db(df, "MeasurementOrFact", fields)
}

#' Extract DNA-derived data from an `occurrence_db()` result
#'
#' @description
#' Convenience wrapper: `unnest_extension_db(df, "DNADerivedData", fields)`. See
#' [measurements_db()] for the `MeasurementOrFact` equivalent.
#'
#' @inheritParams unnest_extension_db
#' @return a `tibble`, one row per DNA-derived-data record.
#' @export
dna_db <- function(df, fields = "id") {
    unnest_extension_db(df, "DNADerivedData", fields)
}
