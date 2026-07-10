# Short aliases for the cli functions used for user-facing messages throughout the package
.w <- cli::cli_alert_warning
.s <- cli::cli_alert_success
.a <- cli::cli_abort

# URIs used by the `extensions` struct field of the open-data parquet dataset
# to identify which kind of extension record a given entry is
# (see https://github.com/iobis/obis-open-data)
.ext_uri <- function(name) {
    map <- c(
        MeasurementOrFact = "http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact",
        DNADerivedData = "http://rs.gbif.org/terms/1.0/DNADerivedData"
    )
    assertthat::assert_that(
        all(name %in% names(map)),
        msg = .a("`extensions`/`hasextensions` should be one of {.var {names(map)}}")
    )
    unname(map[name])
}

# Darwin Core rank terms present in the `interpreted` struct, used to match
# `scientificname` against any taxonomic rank (not only species-level names)
.taxon_rank_fields <- function() {
    c("kingdom", "phylum", "class", "order", "family", "genus", "species")
}

# Curated set of `interpreted` fields selected when `fields` is not supplied.
# The `interpreted` struct has ~270 fields and OBIS's own API returns a
# different sparse subset per query (whichever fields have data), so there is
# no fixed set that reproduces robis::occurrence()'s output exactly; this is a
# reasonably complete, fast-to-scan default. Pass `fields = "all"` for the full
# struct instead.
.default_fields <- function() {
    c(
        "scientificName", "scientificNameAuthorship", "scientificNameID", "aphiaid",
        "kingdom", "phylum", "class", "order", "family", "genus", "species", "taxonRank",
        "decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters",
        "depth", "minimumDepthInMeters", "maximumDepthInMeters",
        "eventDate", "date_year", "date_mid", "day", "month", "year",
        "country", "countryCode", "locality", "waterBody",
        "basisOfRecord", "occurrenceID", "occurrenceStatus", "catalogNumber",
        "institutionCode", "collectionCode", "recordedBy", "identifiedBy",
        "individualCount", "bathymetry", "shoredistance", "sst", "sss", "marine"
    )
}
