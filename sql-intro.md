# A brief SQL introduction (with DuckDB)

`robisdb`'s higher-level functions (`occurrence_db()`, `checklist_db()`, the `select_db()`/
`filter_db()`/... query builder) all end up building and running a SQL query behind the
scenes, and `show_sql()` will print it for you. This is a short, practical primer on
reading and writing that SQL yourself, in case you want to go beyond what those functions
cover, using `DBI::dbGetQuery()` (or this package's [`get_query()`](R/connect.R)) directly.

Every example below assumes a query against the `obis` view/table this package creates,
whose relevant columns for these examples are: `dataset_id`, `flags`, `dropped`, `absence`,
and a nested `interpreted` struct holding fields like `scientificName`, `country`, `depth`,
`aphiaid`, `date_mid`, etc. (see the README for the full picture).

The `obis` table is created by the package by calling:

```sql
create view obis as
  select <which columns were passed to argument `select`>
  from read_parquet('<the path to the Parquet dataset, either locally or remote>')
  where <which rows, according to argument `where`>;
```

> [!NOTE]
> SQL code can be written UPPERCASE (more common), but also lowercase (as we use here).

## The basic shape of a query

Almost every query you'll write has this shape:

```sql
select <which columns>
from <which table>
where <which rows>;
```

For example:

```sql
select interpreted.scientificName, interpreted.depth
from obis
where interpreted.country = 'Brazil';
```

This reads as: "give me the `scientificName` and `depth` fields, from the `obis` table, but
only for rows where `country` is `'Brazil'`." SQL is not case-sensitive for keywords
(`select`/`SELECT` are the same), but string values *are* case-sensitive (`'Brazil'` and
`'brazil'` are different) and must be single-quoted.

> [!TIP]
> You could also directly create your table in the call. For example, working with the `speciesgrids`:
> ```sql
> select * -- selects every column
> from read_parquet('s3://obis-products/speciesgrids/h3_7')
> where species = 'Minuca rapax';
> ```

## Selecting columns

- `select *` -- every column (often expensive on wide/nested tables, see the README).
- `select colA, colB` -- just the columns you name.
- `select colA as some_name` -- rename (`AS`) a column in the output.
- Nested/struct fields are accessed with a dot, e.g. `interpreted.scientificName` -- this is
  the DuckDB-specific part; plain flat tables wouldn't need it.

```sql
select
    interpreted.scientificName,
    interpreted.aphiaid as taxon_id
from obis;
```

## Filtering rows: `where`

Comparison operators: `=`, `<>` (not equal), `<`, `<=`, `>`, `>=`.

```sql
select * from obis where interpreted.depth > 200;
```

Combine conditions with `and`/`or`, and negate with `not`:

```sql
select * from obis
where interpreted.depth > 200 and interpreted.country = 'Brazil';
```

Match against a list of values with `in`:

```sql
select * from obis where interpreted.aphiaid in (955271, 137094);
```

Pattern-match text with `like` (`%` = any characters, `_` = one character):

```sql
select * from obis where interpreted.scientificName like 'Abra %';
```

Check for missing values with `is null`/`is not null` (never use `= null`, it doesn't work):

```sql
select * from obis where interpreted.depth is not null;
```

## Sorting and limiting

```sql
select interpreted.scientificName, interpreted.depth
from obis
order by interpreted.depth desc   -- desc = largest first; asc (default) = smallest first
limit 10;                          -- only the first 10 rows
```

`limit` is especially useful while exploring: it lets DuckDB stop scanning early instead of
reading the whole dataset (this is exactly what `head()` in this package does).

## Aggregating: `group by`

To summarize instead of listing individual rows, pair an aggregate function
(`count(*)`, `sum(x)`, `avg(x)`, `min(x)`, `max(x)`) with `group by`:

```sql
select
    interpreted.country,
    count(*) as n,
    avg(interpreted.depth) as mean_depth
from obis
group by interpreted.country
order by n desc;
```

This is one row per distinct `country`, with `n` counting how many `obis` rows had that
country and `mean_depth` averaging their depths. **Every non-aggregated column in `select`
must also appear in `group by`** -- this is exactly what `checklist_db()` does internally,
grouping by every taxonomic field and counting records per taxon.

## Reserved words and quoting identifiers

Some words are reserved by SQL for its own syntax (`select`, `from`, `where`, `order`,
`group`, `table`, ...) and can't be used as a bare column name without extra care. This
comes up in practice with the Darwin Core taxonomic rank `order`, which appears as a plain
column name in some of OBIS's datasets. In `open-data`, `order` lives nested inside
`interpreted`, so `interpreted.order` already works fine; the dot access disambiguates it
from the `order by` keyword, no special handling needed. But in a flatter dataset like
`speciesgrids`, `order` is a bare, top-level column, and this collides:

```sql
select species, order from obis;
-- Parser Error: syntax error at or near "from"
```

The fix is to wrap the identifier in **double quotes**:

```sql
select species, "order" from obis;   -- works
```

The rule worth remembering: **double quotes (`"..."`) are for identifiers** (column/table
names), **single quotes (`'...'`) are for string values** -- e.g. `"order" = 'Cardiida'`, not
the other way around. Mixing them up is a very common source of confusing errors.

This package's own query-builder verbs (`select_db()`/`filter_db()`/...) pass bare symbols
straight through as identifiers, so a bare `order` doesn't work there either -- and since
`order` also happens to be a base R function, you'll actually hit `order` being *found* by
the variable-lookup fallback (see `?manual_query`) and rejected, rather than a SQL error:

```
Variable `order` found in the calling environment is not an atomic vector; cannot inline into SQL
```

If you need to select a reserved-word column through the query builder, write the quotes
into the R symbol itself with backticks, `` `"order"` `` is an R symbol whose name
literally is `"order"` (quote characters included), which flows through unchanged:

```r
con_sg |> select_db(species, `"order"`) |> collect_db()
```

Simpler in most cases: just drop to raw SQL with `get_query()`/`send_query()` (see below).

## DuckDB extras used throughout this package

A few DuckDB-specific functions/behaviors you'll see if you read the SQL `show_sql()` prints:

- `list_contains(a_list_column, value)` / `len(a_list_column)` -- work with `LIST` columns
  (e.g. `flags`, `node_ids`), which aren't a standard-SQL concept.
- `list_filter(a_list, x -> <condition on x>)` / `list_transform(a_list, x -> <expr on x>)`
  -- DuckDB's lambda syntax for filtering/transforming list elements, used for the
  `mof`/`dna` extension columns.
- `ST_Intersects(geom_a, geom_b)`, `ST_GeomFromText('<WKT>')`, `ST_Buffer(geom, distance)`
  -- spatial functions from DuckDB's `spatial` extension (see `filter_spatial_db()`).
- `struct_extract(a_struct, 'field')` -- the function-call form of `a_struct.field`.

## Running SQL yourself

```r
con <- connect_opendata_local("~/data/obis-open-data")

# get_query()/send_query() accept the robisdb_conn directly (or a raw connection, if you have one)
get_query(con, "
    select interpreted.country, count(*) as n
    from obis
    where interpreted.aphiaid = 955271
    group by interpreted.country
    order by n desc;
")
```

Or build the same thing without writing SQL text by hand, using this package's query
builder (see the README and `?manual_query`):

```r
con |>
    filter_db(interpreted.aphiaid == 955271) |>
    group_by_db(interpreted.country) |>
    summarize_db(n = n()) |>
    show_sql() |>   # prints the exact SQL being run -- a good way to learn by comparison
    collect_db()
```

## Learning more

This covers the basics used throughout this package's own code and generated queries.
DuckDB's own docs are excellent for going further:
[SQL Introduction](https://duckdb.org/docs/sql/introduction),
[full SQL function reference](https://duckdb.org/docs/sql/functions/overview).
