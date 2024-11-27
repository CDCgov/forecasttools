#' Compare two quantities `a` and `b` with a given
#' `comparison operator`,
#' returning a single `TRUE` if `b` is
#' `NULL`.
#'
#' Useful for letting `NULL` reflect "all values"
#' if [dplyr::filter()] calls. Internal function.
#'
#' @param a First set of values for the comparison
#' @param comparison_operator Comparison operator for the comparison,
#' as a string, e.g. `"=="`, `">="` `"%in%"`.
#' @param b Second set of values for the comparison, or `NULL`.
#' @return A logical vector. Equivalent to `b {comparison_operator} a`
#' if `b` is not `NULL` and to `TRUE` if `b` is `NULL`.
nullable_comparison <- function(a,
                                comparison_operator,
                                b) {
  comparison_func <- getFunction(comparison_operator)
  return(
    if (!is.null(b)) {
      comparison_func(a, b)
    } else {
      TRUE
    }
  )
}


#' Construct a [soql::soql_where()] component of a SOQL query
#' programmatically if and only if the `where_value` is not `NULL`.
#'
#' If the where_value is `NULL`, return all rows.
#'
#' @param soql_list A `soql` query object, which
#' can be piped in. If one hasn't been
#' created yet, use or pipe in [soql::soql()].
#' @param column column name for the [soql::soql_where()] component
#' of the query, as a string.
#' @param comparison_operator comparison operator for the
#' [soql::soql_where()] component of the query, as
#' @return A new [soql::soql()] object with the filter added, or
#' simply the input object if `where_value` is `NULL`.
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


#' Return a [soql::soql_where()] construct
#' for a given column being in a vector of `match_values`,
#' or no addition filter if the vector of `match_values`
#' is `NULL`.
#'
#' @param soql_list A `soql` query object, which
#' can be piped in. If one hasn't been
#' created yet, use or pipe in [soql::soql()].
#' @param column The column to filter on
#' @param match_values A vector of values that column
#' must match, or NULL to add no filter on that column.
#' @return A new [soql::soql()] object with the filter added,
#' or simply the input object if `match_value` is `NULL`.
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

#'
#'
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
