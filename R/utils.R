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
nullable_comparison <- function(a, comparison_operator, b) {
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
soql_nullable_where <- function(
  soql_list,
  column,
  comparison_operator,
  where_value
) {
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
        paste(unique(columns), collapse = ",")
      )
    } else {
      soql_list
    }
  )
}

#' Read from or write to tabular files, with
#' file format inferred from the file extension.
#'
#' @param table Table to write (`write_tabular_file` only).
#' @param path_to_file Path to the file to read/write.
#' Must have extension `.tsv`, `.csv`, or `.parquet`
#' (not case-sensitive).
#' @param ... Additional keyword arguments passed to the
#' file reader/writer function, which will be one of
#' [readr::read_csv()] / [readr::write_csv()],
#' [readr::read_tsv()] / [readr::write_tsv()], and
#' [arrow::read_parquet()] / [arrow::write_parquet()],
#' depending on the file format.
#' @return For `read_tabular_file`, the result of
#' reading in the file, as a
#' [`tibble`][tibble::tibble()]. For `write_tabular_file`,
#' nothing, saving the tabular to disk as a side effect.
#' @export
read_tabular_file <- function(path_to_file, ...) {
  file_format <- fs::path_ext(path_to_file)

  file_format <- tolower(file_format)

  file_readers <- c(
    "tsv" = readr::read_tsv,
    "csv" = readr::read_csv,
    "parquet" = arrow::read_parquet
  )

  checkmate::assert_names(file_format, subset.of = names(file_readers))

  file_reader <- file_readers[[file_format]]

  return(file_reader(path_to_file, ...))
}

#' Check if an object is converted to an Arrow ExtensionArray
#'
#' This function tests whether an object is converted to an Arrow ExtensionArray
#' rather than a regular Arrow Array.
#'
#' @param x An R object to check
#' @return A logical value: TRUE if the object can is converted to an Arrow
#' ExtensionArray, FALSE otherwise.
#' @noRd
#' @examples
#' # Arrow ExtensionArray
#' is_extension_array(fs::path(letters))
#'
#' # Arrow Array
#' # Regular R vector
#' is_extension_array(1:10)
is_extension_array <- function(x) {
  "ExtensionArray" %in% class(arrow::as_arrow_array(x))
}

#' @param simplify_column_types If `TRUE`, attempts to convert extension arrays
#' (e.g. fs paths, glue strings) to plain base R vectors before writing.
#' @rdname read_tabular_file
#' @export
write_tabular_file <- function(
  table,
  path_to_file,
  simplify_column_types = TRUE,
  ...
) {
  file_format <- fs::path_ext(path_to_file)

  file_writers <- c(
    "tsv" = readr::write_tsv,
    "csv" = readr::write_csv,
    "parquet" = arrow::write_parquet
  )

  checkmate::assert_names(file_format, subset.of = names(file_writers))

  file_writer <- file_writers[[file_format]]

  if (simplify_column_types) {
    table <- dplyr::mutate(
      table,
      dplyr::across(tidyselect::where(is_extension_array), vctrs::vec_data)
    )
  }

  file_writer(table, path_to_file, ...)
}

#' @rdname read_tabular_file
#' @export
write_tabular <- write_tabular_file

#' @rdname read_tabular_file
#' @export
read_tabular <- read_tabular_file

#' Get limits spanning a set of values
#' that are symmetric about a central value
#'
#' This is useful for making the axis range of a
#' plot symmetric about a key central value.
#'
#' Given a center point `center` and a set of values `values`, returns
#' `c(center - x, center + x)` where `x` is the entry of `values` that is
#' farthest from `center` (i.e. `x <- max(abs(values - center))`) on the
#' transformed scale.
#'
#' If a custom `center` point is not specified, `sym_limits()`
#' centers values around 0 on the transformed scale.
#'
#' @param values Vector of values.
#' @param transform Transformation to apply to the values.
#' Passed to [scales::as.transform()]. Default `"identity"`.
#' @param center Custom center value for the span
#' on the _untransformed_ scale. If not specified,
#' the span will be centered at 0 on the _transformed_ scale.
#' @return a length-two vector whose entries are
#' symmetric about `center` on the transformed scale
#' and span the range of `values`.
#' @examples
#' sym_limits(c(-5.1, 2.2, 7.1))
#'
#' # centered at 1, which corresponds to 0 on the
#' # transformed scale
#' sym_limits(values = c(0.4, 10), transform = "log10")
#'
#' sym_limits(values = c(2, 9), transform = "sqrt", center = 4)
#'
#' library(ggplot2)
#' library(tibble)
#' data <- tibble(x = 1:5, y = rnorm(5, mean = 2))
#' plot <- ggplot(
#'     data = data,
#'     mapping = aes(x = x, y = y)) +
#'     geom_hline(yintercept = 2, size = 2, linetype = "dashed") +
#'     geom_point(size = 4) +
#'     coord_cartesian(ylim = sym_limits(data$y, center = 2)) +
#'     theme_forecasttools()
#' plot
#'
#' data_log <- tibble(x = 1:5, y = 10^rnorm(5))
#' plot_log <- ggplot(
#'     data = data_log,
#'     mapping = aes(x = x, y = y)) +
#'     geom_hline(yintercept = 1, size = 2, linetype = "dashed") +
#'     geom_point(size = 4) +
#'     scale_y_continuous(transform = "log10") +
#'     coord_cartesian(ylim = sym_limits(data_log$y, transform = "log10")) +
#'     theme_forecasttools()
#' plot_log
#' @export
sym_limits <- function(values, transform = "identity", center = NULL) {
  checkmate::assert_numeric(values, min.len = 1)
  transform_fn <- scales::as.transform(transform)
  transformed_values <- transform_fn$transform(values)

  if (!is.null(center)) {
    transformed_center <- transform_fn$transform(center)
  } else {
    transformed_center <- 0
  }
  span <- max(abs(transformed_values - transformed_center))

  return(transform_fn$inverse(transformed_center + c(-span, span)))
}
