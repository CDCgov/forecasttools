#' Compare two quantities `a` and `b` with a given
#' `comparison operator`,
#' returning a single `TRUE` if `b` is
#' `NULL`.
#'
#' Useful for letting `x {comparison} NULL` mean "match all values"
#' in calls to [dplyr::filter()] and similar functions.
#'
#' @param a First set of values for the comparison
#' @param comparison_operator Comparison operator for the comparison,
#' as a string, e.g. `"=="`, `">="` `"%in%"`.
#' @param b Second set of values for the comparison, or `NULL`.
#' @return A logical vector. Equivalent to `b {comparison_operator} a`
#' if `b` is not `NULL` and to `TRUE` if `b` is `NULL`.
#'
#' @examples
#'
#' x <- 6
#' nullable_comparison(5, ">", x)
#'
#' x <- NULL
#' nullable_comparison(5, ">", x)
#'
#' df <- tibble::tibble(y = 1:6)
#' x <- 3
#' df |> dplyr::filter(
#'   nullable_comparison(y, ">", !!x),
#'   y < 5
#' )
#' x <- NULL
#' df |> dplyr::filter(
#'   nullable_comparison(y, ">", !!x),
#'   y < 5
#' )
#'
#' @export
nullable_comparison <- function(a,
                                comparison_operator,
                                b) {
  comparison_func <- methods::getFunction(comparison_operator)
  return(
    if (!is.null(b)) {
      comparison_func(a, b)
    } else {
      TRUE
    }
  )
}


#' Add a [soql::soql_where()] statement to a SOQL query,
#' or return the original query if `where_value` is  `NULL`.
#'
#' @param soql_list A `soql` query object, which
#' can be piped in. If one hasn't been
#' created yet, use or pipe in [soql::soql()].
#' @param column column name for the [soql::soql_where()] component
#' of the query, as a string.
#' @param comparison_operator comparison operator for the
#' [soql::soql_where()] component of the query, as a string.
#' @param where_value A value for the comparison, or `NULL`
#' for no filtering.
#' @return A new [soql::soql()] object with the filter added, or
#' simply the input object if `where_value` is `NULL`.
#' @export
soql_nullable_where <- function(soql_list,
                                column,
                                comparison_operator,
                                where_value) {
  return(
    if (!is.null(where_value)) {
      soql::soql_where(
        soql_list,
        glue::glue(paste0(
          "{column} ",
          "{comparison_operator} ",
          "'{where_value}'"
        ))
      )
    } else {
      soql_list
    }
  )
}


#' Add an "is in" statement to a SOQL query, or return the original
#' query if `match_values` is  `NULL`.
#'
#' An is in statement is a [soql::soql_where()] statement
#' that requires the values of a given column to match one of the
#' entries of a vector of `match_values`.
#'
#' @param soql_list A `soql` query object, which
#' can be piped in. If one hasn't been
#' created yet, use or pipe in [soql::soql()].
#' @param column The column to filter on
#' @param match_values A vector of values that column
#' must match, or `NULL` for no filtering.
#' @return A new [soql::soql()] object with the filter added,
#' or simply the input object if `match_value` is `NULL`.
#' @export
soql_nullable_is_in <- function(soql_list, column, match_values) {
  if (is.null(match_values)) {
    return(soql_list)
  } else {
    query <- glue::glue(
      "{column}='{unique(match_values)}'"
    ) |>
      paste(collapse = " OR ")
    return(soql::soql_where(soql_list, query))
  }
}

#' Add a [soql::soql_select()] statement to a query
#' to select a set of columns `columns`, or return
#' the original query if `columns` is `NULL`.
#'
#' @param soql_list A `soql` query object, which
#' can be piped in. If one hasn't been
#' created yet, use or pipe in [soql::soql()].
#' @param columns The columns to select, or `NULL`.
#' @return A new [soql::soql()] object with the selection statement
#' added, or the input object if `columns` is `NULL`.
#' @export
soql_nullable_select <- function(soql_list, columns) {
  return(
    if (!is.null(columns)) {
      soql::soql_select(
        soql_list,
        paste(unique(columns),
          collapse = ","
        )
      )
    } else {
      soql_list
    }
  )
}

#' @rdname write_tabular_file
read_tabular_file <- function(path_to_file,
                              ...) {
  file_format <- fs::path_ext(path_to_file)

  file_format <- tolower(file_format)

  file_readers <- c(
    "tsv" = readr::read_tsv,
    "csv" = readr::read_csv,
    "parquet" = arrow::read_parquet
  )

  checkmate::assert_names(file_format,
    subset.of = names(file_readers)
  )

  file_reader <- file_readers[[file_format]]

  return(file_reader(path_to_file, ...))
}

#' Read from or write to tabular files, with
#' file format inferred from the file extension.
#'
#' @param table Table to write (`write_tabular_file` only).
#' @param path_to_file Path to the file to read/write.
#' @param ... Additional keyword arguments passed to the
#' file reader/writer function, which will be one of
#' [readr::read_csv()] / [readr::write_csv()],
#' [readr::read_tsv()] / [readr::write_tsv()], and
#' [arrow::read_parquet()] / [arrow::write_parquet()],
#' depending on the `file_format`.
#' @return For `read_tabular_file`, the result of
#' reading in the file, as a
#' [`tibble`][tibble::tibble()]. For `write_tabular_file`,
#' nothing, saving the tabular to disk as a side effect.
#' @export
write_tabular_file <- function(table,
                               path_to_file,
                               ...) {
  file_format <- fs::path_ext(path_to_file)

  file_writers <- c(
    "tsv" = readr::write_tsv,
    "csv" = readr::write_csv,
    "parquet" = arrow::write_parquet
  )

  checkmate::assert_names(file_format,
    subset.of = names(file_writers)
  )

  file_writer <- file_writers[[file_format]]

  file_writer(table, path_to_file, ...)
}
