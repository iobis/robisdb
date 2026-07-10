#' Manual query-builder verbs for `robisdb_conn`
#'
#' @description
#' A small, self-contained set of dplyr-style verbs for building a SQL query against the
#' `obis` view step by step. Unlike [duckplyr], this does not attempt to translate general
#' R data-manipulation semantics through a relational engine, which does not cope well with
#' this dataset's deeply nested `interpreted`/`extensions` struct columns -- it is a thin,
#' literal R-expression-to-SQL-text translator instead. Nothing is executed until
#' [collect_db()] (or [show_sql()], which only prints the query).
#'
#' Revisited: `duckplyr`'s `dd::unnest()` does let plain `dplyr::select()`/`filter()`
#' reach the flat `interpreted`/`source` structs directly, with full DuckDB pushdown --
#' but it doesn't reach the `extensions` struct-of-list-of-struct (still needs the same
#' flattening `.sql_extension_select()` does), can't call arbitrary DuckDB/extension SQL
#' functions at all (`ST_Intersects`, `h3_*` -- there is no escape hatch, it's a hard R
#' error), and silently materializes the *entire* table into R on any unsupported
#' operation. That's not a fit for this dataset's scale, so this file remains the
#' supported query path; see [to_duckplyr()] for an explicitly experimental alternative.
#'
#' `select_db()`, `mutate_db()`, `filter_db()`, `group_by_db()`,
#' `summarize_db()`/`summarise_db()`, and `collect_db()` are named with a `_db` suffix
#' (rather than the bare dplyr verb names) specifically so they are never masked by
#' `dplyr::filter()`/`mutate()`/etc. when both packages are attached.
#'
#' `select_db()` *replaces* the select list (like `dplyr::select()`), while `mutate_db()`
#' *appends* to whatever is currently selected (starting from `*` by default). Since the
#' underlying `interpreted`/`source`/`extensions` structs are very wide, an unnarrowed
#' `select *` can be slow to materialize -- call `select_db()` before `collect_db()` to
#' name only the fields you need.
#'
#' @details
#' Bare symbols in an expression (e.g. `interpreted.depth`) are treated as column
#' identifiers, *unless* a variable of that name exists in the calling environment, in
#' which case its value is evaluated and inlined as a SQL literal (or an `IN` list, for a
#' vector) -- e.g. `depth_limit <- 100; filter_db(con, interpreted.depth <= depth_limit)`.
#' Because the dataset's real fields are qualified names like `interpreted.depth`, which
#' are never plausible variable names, this rarely creates ambiguity in practice; it can in
#' principle shadow an *unqualified* top-level column (`id`, `flags`, `dropped`, `absence`)
#' if a same-named variable happens to be in scope.
#'
#' A bare `order`, `group`, or `references` (real column names in some datasets, e.g.
#' `speciesgrids`, that are also reserved SQL keywords) is always treated as a
#' double-quoted column identifier, before the variable-lookup above is even attempted --
#' otherwise DuckDB would reject them unquoted. Qualified forms like `interpreted.order`
#' don't need this and are unaffected.
#'
#' Known limitations: `filter_db()` always emits to `WHERE` (no post-aggregation `HAVING`
#' support); `group_by_db()` replaces the grouping columns on each call and only takes
#' effect once `summarize_db()`/`summarise_db()` is called; two `summarize_db()` calls in
#' one chain are not supported (no subquery nesting); `^`, `%%`, `%/%` are not translated.
#'
#' @param x a `robisdb_conn`.
#' @param ... for `select_db()`, (optionally named) column expressions, replacing the
#'   select list; for `mutate_db()`/`summarize_db()`/`summarise_db()`, named expressions
#'   (the resulting column aliases); for `filter_db()`, one or more boolean expressions
#'   (AND-ed together); for `group_by_db()`, bare (optionally named) grouping columns.
#' @param n for `head.robisdb_conn()`, the number of rows to sample (default `10`).
#' @return `select_db()`, `mutate_db()`, `filter_db()`, `group_by_db()` return the modified
#'   `robisdb_conn`. `summarize_db()`/`summarise_db()` return the modified `robisdb_conn`
#'   with the select list replaced by the grouping columns and aggregate expressions.
#'   `collect_db()` runs the built query and returns a `tibble`. `show_sql()` prints the
#'   query and returns `x` invisibly. `head.robisdb_conn()`/`glimpse()`/`colnames()` ignore
#'   any accumulated query state entirely: `head.robisdb_conn()`/`glimpse()` always sample
#'   `select * from obis limit n`, and `colnames()` (via a `dimnames.robisdb_conn()` method)
#'   always returns every column name in `obis`, cheaply, via `describe obis` (no rows read).
#' @name manual_query
NULL

#' @rdname manual_query
#' @export
select_db <- function(x, ...) UseMethod("select_db")

#' @rdname manual_query
#' @export
mutate_db <- function(x, ...) UseMethod("mutate_db")

#' @rdname manual_query
#' @export
filter_db <- function(x, ...) UseMethod("filter_db")

#' @rdname manual_query
#' @export
group_by_db <- function(x, ...) UseMethod("group_by_db")

#' @rdname manual_query
#' @export
summarize_db <- function(x, ...) UseMethod("summarize_db")

#' @rdname manual_query
#' @export
summarise_db <- function(x, ...) UseMethod("summarise_db")

#' @rdname manual_query
#' @export
collect_db <- function(x, ...) UseMethod("collect_db")

#' @rdname manual_query
#' @export
show_sql <- function(x, ...) UseMethod("show_sql")

#' @rdname manual_query
#' @export
glimpse <- function(x, ...) UseMethod("glimpse")

# Unique sentinel used only for identity comparison, to distinguish "the symbol isn't
# bound anywhere visible" from "the symbol legitimately evaluates to NULL/NA"
.sym_not_found <- local({
    e <- new.env()
    e
})

# DuckDB `reserved` keywords (per duckdb_keywords()) that also happen to be real column
# names in datasets this package queries (speciesgrids' `order`/`group`/`references`, among
# others via `source`/`interpreted`) -- bare references to these always need quoting, so
# they're recognized before any variable-lookup is attempted. Only applies to the exact bare
# word: qualified forms like `interpreted.order` are unaffected (and don't need this at all,
# since the dotted struct-field syntax is unambiguous to DuckDB regardless of `order` being
# reserved).
.reserved_column_names <- c("order", "group", "references")

# Recursive R-expression -> SQL-text translator. A bare symbol is resolved against `env`
# (the caller's environment) first; only if that lookup fails is it treated as a column
# identifier. This is a tree walk, not text substitution on deparse() output, so operators
# inside string literals are never touched.
.expr_to_sql <- function(expr, env) {
    if (is.null(expr)) {
        return("NULL")
    }
    if (is.symbol(expr)) {
        nm <- as.character(expr)
        if (nm == "") return("NULL")
        if (nm %in% .reserved_column_names) return(paste0('"', nm, '"'))
        val <- tryCatch(eval(expr, envir = env), error = function(e) .sym_not_found)
        if (identical(val, .sym_not_found)) return(nm)
        if (!is.atomic(val)) {
            .a("Variable `{nm}` found in the calling environment is not an atomic vector; cannot inline into SQL")
        }
        return(.value_to_sql(val))
    }
    if (is.call(expr)) {
        return(.translate_call_expr(expr, env))
    }
    if (is.atomic(expr)) {
        return(.literal_to_sql(expr))
    }
    deparse(expr)
}

# Formats an already-evaluated R value (from a resolved variable) as SQL: a scalar
# becomes a single literal, a vector becomes a parenthesized comma-separated list
# (valid both as a tuple and, unwrapped by callers where needed, as an IN-list)
.value_to_sql <- function(val) {
    if (length(val) == 1) return(.literal_to_sql(val))
    paste0("(", paste(vapply(val, .literal_to_sql, character(1)), collapse = ", "), ")")
}

# R comparison operator -> SQL operator (mostly 1:1; `!=` is spelled `<>`)
.comparison_ops <- c("==" = "=", "!=" = "<>", "<" = "<", "<=" = "<=", ">" = ">", ">=" = ">=")

# Translates one parsed R call (e.g. `x > 10`, `a & b`, `x %in% y`) into SQL text,
# recursing into `.expr_to_sql()` for each argument. Falls through to
# `.translate_fun_call()` for anything that isn't a recognized operator, so ordinary
# function calls (`sum(x)`, `round(x, 1)`, ...) are treated as SQL function calls.
.translate_call_expr <- function(expr, env) {
    op <- as.character(expr[[1]])
    args <- as.list(expr)[-1]

    if (op %in% names(.comparison_ops)) {
        return(paste(.expr_to_sql(args[[1]], env), .comparison_ops[[op]], .expr_to_sql(args[[2]], env)))
    }

    switch(op,
        "&" = ,
        "&&" = return(paste0("(", .expr_to_sql(args[[1]], env), " AND ", .expr_to_sql(args[[2]], env), ")")),
        "|" = ,
        "||" = return(paste0("(", .expr_to_sql(args[[1]], env), " OR ", .expr_to_sql(args[[2]], env), ")")),
        "!" = return(paste0("NOT (", .expr_to_sql(args[[1]], env), ")")),
        "%in%" = return(.translate_in(args[[1]], args[[2]], env)),
        "(" = return(paste0("(", .expr_to_sql(args[[1]], env), ")")),
        "$" = return(paste0(.expr_to_sql(args[[1]], env), ".", .expr_to_sql(args[[2]], env))),
        "+" = ,
        "-" = ,
        "*" = ,
        "/" = {
            if (length(args) == 1) return(paste0(op, .expr_to_sql(args[[1]], env)))
            return(paste0("(", .expr_to_sql(args[[1]], env), " ", op, " ", .expr_to_sql(args[[2]], env), ")"))
        }
    )

    .translate_fun_call(args, op, env)
}

# R aggregate-function name -> DuckDB name, for the few that don't already match
.agg_fun_map <- c(mean = "avg", sd = "stddev_samp", var = "var_samp")

# Translates a function call not recognized as an operator by .translate_call_expr():
# `n()` (zero-arg) becomes `count(*)`; a handful of R aggregate names are remapped via
# .agg_fun_map; everything else (sum/min/max/count/round/abs/...) passes through
# unchanged, since it already matches DuckDB's function name
.translate_fun_call <- function(args, fn, env) {
    if (fn == "n" && length(args) == 0) return("count(*)")
    sql_fn <- if (fn %in% names(.agg_fun_map)) .agg_fun_map[[fn]] else fn
    translated_args <- vapply(args, .expr_to_sql, character(1), env = env)
    paste0(sql_fn, "(", paste(translated_args, collapse = ", "), ")")
}

# Translates `lhs %in% rhs`. If `rhs` is a literal `c(...)` call, each element is
# translated individually; otherwise `rhs` is resolved via .expr_to_sql() (which
# evaluates it in `env` if it isn't a column reference) and wrapped in parentheses,
# covering both a bare variable holding a vector and other arbitrary expressions
.translate_in <- function(lhs, rhs, env) {
    lhs_sql <- .expr_to_sql(lhs, env)
    if (is.call(rhs) && identical(rhs[[1]], as.name("c"))) {
        vals_sql <- vapply(as.list(rhs)[-1], .expr_to_sql, character(1), env = env)
        return(paste0(lhs_sql, " IN (", paste(vals_sql, collapse = ", "), ")"))
    }
    rhs_sql <- .expr_to_sql(rhs, env)
    if (!startsWith(rhs_sql, "(")) rhs_sql <- paste0("(", rhs_sql, ")")
    paste0(lhs_sql, " IN ", rhs_sql)
}

# Formats a single atomic R literal as SQL text: NA -> NULL, strings quoted via
# .sql_quote(), logicals as lowercase true/false, numerics via format() (deparse()
# would emit invalid SQL like "2L" for an integer)
.literal_to_sql <- function(x) {
    if (length(x) != 1) return(paste(vapply(x, .literal_to_sql, character(1)), collapse = ", "))
    if (is.na(x)) return("NULL")
    if (is.character(x)) return(.sql_quote(x))
    if (is.logical(x)) return(if (x) "true" else "false")
    if (is.numeric(x)) return(format(x, scientific = FALSE, trim = TRUE, decimal.mark = "."))
    deparse(x)
}

# Captures `...` as a list of unevaluated expressions (with names, for aliasing),
# without evaluating them -- the base-R alternative to rlang::enquos()
.capture_dots <- function(...) {
    eval(substitute(alist(...)))
}

#' @rdname manual_query
#' @export
select_db.robisdb_conn <- function(x, ...) {
    caller_env <- parent.frame()
    exprs <- .capture_dots(...)
    if (length(exprs) == 0) .a("`select_db()` requires at least one column expression")
    nms <- names(exprs)
    if (is.null(nms)) nms <- rep("", length(exprs))
    translated <- vapply(exprs, .expr_to_sql, character(1), env = caller_env)
    x$query$select <- ifelse(nzchar(nms), paste0(translated, ' AS "', nms, '"'), translated)
    x
}

#' @rdname manual_query
#' @export
mutate_db.robisdb_conn <- function(x, ...) {
    caller_env <- parent.frame()
    exprs <- .capture_dots(...)
    nms <- names(exprs)
    if (is.null(nms) || any(!nzchar(nms))) {
        .a("All arguments to `mutate_db()` must be named, e.g. `mutate_db(con, new_col = interpreted.depth * 2)`")
    }
    translated <- vapply(exprs, .expr_to_sql, character(1), env = caller_env)
    x$query$select <- c(x$query$select, paste0(translated, ' AS "', nms, '"'))
    x
}

#' @rdname manual_query
#' @export
filter_db.robisdb_conn <- function(x, ...) {
    caller_env <- parent.frame()
    exprs <- .capture_dots(...)
    translated <- vapply(exprs, .expr_to_sql, character(1), env = caller_env)
    x$query$where <- c(x$query$where, translated)
    x
}

#' @rdname manual_query
#' @export
group_by_db.robisdb_conn <- function(x, ...) {
    caller_env <- parent.frame()
    exprs <- .capture_dots(...)
    nms <- names(exprs)
    if (is.null(nms)) nms <- rep("", length(exprs))
    translated <- vapply(exprs, .expr_to_sql, character(1), env = caller_env)
    names(translated) <- nms
    x$query$group_by <- translated
    x
}

#' @rdname manual_query
#' @export
summarize_db.robisdb_conn <- function(x, ...) {
    caller_env <- parent.frame()
    exprs <- .capture_dots(...)
    if (length(exprs) == 0) .a("`summarize_db()` requires at least one named aggregate expression")
    nms <- names(exprs)
    if (is.null(nms) || any(!nzchar(nms))) {
        .a(c(
            "All arguments to `summarize_db()` must be named.",
            "i" = "e.g. `summarize_db(con, avg_depth = mean(interpreted.depth))`"
        ))
    }
    translated <- vapply(exprs, .expr_to_sql, character(1), env = caller_env)
    agg_select <- paste0(translated, ' AS "', nms, '"')

    gb <- x$query$group_by
    gb_select <- if (length(gb) > 0) {
        ifelse(nzchar(names(gb)), paste0(gb, ' AS "', names(gb), '"'), unname(gb))
    } else {
        character(0)
    }

    x$query$select <- c(gb_select, agg_select)
    x$query$summarized <- TRUE
    x
}

# Direct function-object alias (not a wrapper that calls summarize_db()), so parent.frame()
# still resolves correctly regardless of which spelling the user calls.
#' @rdname manual_query
#' @export
summarise_db.robisdb_conn <- summarize_db.robisdb_conn

# Assembles the final SQL string from `x$query`; shared by collect_db() (which runs it)
# and show_sql() (which only prints it), so the two are always guaranteed to match
.build_query_sql <- function(x) {
    select_q <- x$query$select
    if (length(select_q) == 0) select_q <- "*"

    query <- paste0("select\n    ", paste(select_q, collapse = ",\n    "), "\nfrom obis")

    if (length(x$query$where) > 0) {
        query <- paste0(query, "\nwhere\n    ", paste(x$query$where, collapse = "\n    and "))
    }

    if (isTRUE(x$query$summarized) && length(x$query$group_by) > 0) {
        query <- paste0(query, "\ngroup by\n    ", paste(unname(x$query$group_by), collapse = ", "))
    } else if (!isTRUE(x$query$summarized) && length(x$query$group_by) > 0) {
        .w("`group_by_db()` was set but `summarize_db()`/`summarise_db()` was never called; grouping has no effect.")
    }

    paste0(query, ";")
}

#' @rdname manual_query
#' @export
collect_db.robisdb_conn <- function(x, ...) {
    tibble::as_tibble(DBI::dbGetQuery(x$connection, .build_query_sql(x)))
}

#' @rdname manual_query
#' @export
show_sql.robisdb_conn <- function(x, ...) {
    cli::cli_h3("Query")
    cli::cli_code(.build_query_sql(x))
    invisible(x)
}

#' @rdname manual_query
#' @export
head.robisdb_conn <- function(x, n = 10L, ...) {
    query <- paste0("select * from obis limit ", as.integer(n), ";")
    tibble::as_tibble(DBI::dbGetQuery(x$connection, query))
}

#' @rdname manual_query
#' @export
glimpse.robisdb_conn <- function(x, ...) {
    sample_df <- tibble::as_tibble(DBI::dbGetQuery(x$connection, "select * from obis limit 10;"))
    pillar::glimpse(sample_df, ...)
    invisible(x)
}

# `colnames()` isn't itself a UseMethod()-based S3 generic (it's a plain function that
# falls back to `dimnames(x)[[2L]]`), but `dimnames` is an internal generic that does
# dispatch -- defining this method is what makes `colnames(x)` work correctly.
#' @rdname manual_query
#' @export
dimnames.robisdb_conn <- function(x) {
    cols <- DBI::dbGetQuery(x$connection, "describe obis;")$column_name
    list(NULL, cols)
}

#' Spatial filtering
#'
#' @description
#' Adds a spatial `WHERE` condition to the query: keep only records whose `column`
#' intersects `geometry` (a WKT string), optionally buffered first. This covers the two
#' common cases -- `ST_Intersects(column, ST_GeomFromText(geometry))`, and the same
#' wrapped in `ST_Buffer()` when `buffer` is supplied -- built with the `spatial`
#' extension, which [connect()] always loads. For anything more elaborate, build the
#' condition yourself and pass it to [filter_db()].
#'
#' @param x a `robisdb_conn`.
#' @param geometry a single WKT geometry string.
#' @param buffer optional buffer distance to apply to `geometry` before intersecting
#'   (in the units of `geometry`'s coordinate system -- degrees, for the unprojected
#'   WGS84 data used throughout this package).
#' @param column the geometry column to filter on (default `"geometry"`, the dataset's
#'   geometry column).
#' @param ... unused.
#' @return the modified `robisdb_conn`.
#' @export
filter_spatial_db <- function(x, ...) UseMethod("filter_spatial_db")

#' @rdname filter_spatial_db
#' @export
filter_spatial_db.robisdb_conn <- function(x, geometry, buffer = NULL, column = "geometry", ...) {
    if (!is.character(geometry) || length(geometry) != 1) .a("`geometry` should be a single WKT string")
    geom_expr <- paste0("ST_GeomFromText(", .sql_quote(geometry), ")")
    if (!is.null(buffer)) {
        if (!is.numeric(buffer) || length(buffer) != 1) .a("`buffer` should be a single number")
        geom_expr <- paste0("ST_Buffer(", geom_expr, ", ", buffer, ")")
    }
    x$query$where <- c(x$query$where, paste0("ST_Intersects(", column, ", ", geom_expr, ")"))
    x
}

#' H3 hexagonal indexing
#'
#' @description
#' Convenience wrappers around DuckDB's `h3` community extension (see
#' <https://duckdb.org/community_extensions/extensions/h3>) for building an H3 index and
#' navigating its resolution hierarchy, each appending one computed column (like
#' [mutate_db()]). Requires the extension to be installed/loaded first, once per
#' connection, via [install_h3()].
#'
#' `h3_index_db()` computes the H3 cell for a lon/lat pair at `resolution` (a coarser
#' `resolution` covers a larger area). `h3_parent_db()`/`h3_children_db()` move an
#' existing H3 cell column up/down the resolution hierarchy -- `h3_children_db()`'s
#' result is a list-column, since one cell has multiple children.
#'
#' @param x a `robisdb_conn`.
#' @param lat,lng bare (unquoted) latitude/longitude column expressions, e.g.
#'   `interpreted.decimalLatitude`.
#' @param cell a bare (unquoted) H3 cell column expression.
#' @param resolution for `h3_index_db()`, the resolution to compute the cell at; for
#'   `h3_parent_db()`/`h3_children_db()`, the (coarser/finer, respectively) resolution to
#'   move `cell` to.
#' @param name the name of the resulting column.
#' @param ... unused.
#' @return the modified `robisdb_conn`.
#' @name h3_index
NULL

#' @rdname h3_index
#' @export
h3_index_db <- function(x, ...) UseMethod("h3_index_db")

#' @rdname h3_index
#' @export
h3_index_db.robisdb_conn <- function(x, lat, lng, resolution, name = "h3_index", ...) {
    caller_env <- parent.frame()
    if (!is.numeric(resolution) || length(resolution) != 1) .a("`resolution` should be a single number")
    lat_sql <- .expr_to_sql(substitute(lat), caller_env)
    lng_sql <- .expr_to_sql(substitute(lng), caller_env)
    expr <- paste0("h3_latlng_to_cell(", lat_sql, ", ", lng_sql, ", ", as.integer(resolution), ")")
    x$query$select <- c(x$query$select, paste0(expr, ' AS "', name, '"'))
    x
}

#' @rdname h3_index
#' @export
h3_parent_db <- function(x, ...) UseMethod("h3_parent_db")

#' @rdname h3_index
#' @export
h3_parent_db.robisdb_conn <- function(x, cell, resolution, name = "h3_parent", ...) {
    caller_env <- parent.frame()
    if (!is.numeric(resolution) || length(resolution) != 1) .a("`resolution` should be a single number")
    cell_sql <- .expr_to_sql(substitute(cell), caller_env)
    expr <- paste0("h3_cell_to_parent(", cell_sql, ", ", as.integer(resolution), ")")
    x$query$select <- c(x$query$select, paste0(expr, ' AS "', name, '"'))
    x
}

#' @rdname h3_index
#' @export
h3_children_db <- function(x, ...) UseMethod("h3_children_db")

#' @rdname h3_index
#' @export
h3_children_db.robisdb_conn <- function(x, cell, resolution, name = "h3_children", ...) {
    caller_env <- parent.frame()
    if (!is.numeric(resolution) || length(resolution) != 1) .a("`resolution` should be a single number")
    cell_sql <- .expr_to_sql(substitute(cell), caller_env)
    expr <- paste0("h3_cell_to_children(", cell_sql, ", ", as.integer(resolution), ")")
    x$query$select <- c(x$query$select, paste0(expr, ' AS "', name, '"'))
    x
}
