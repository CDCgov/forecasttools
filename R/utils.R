#' Compare a vector to a set of valid values,
#' returning a single `TRUE` if the set of valid
#' values is `NULL`.
#'
#' Useful for letting `NULL` reflect "all values"
#' if [dplyr::filter()] calls. Internal function.
#'
#' @param vector Vector of values to test
#' @param valid_values Vector of valid values, possibly
#' `NULL`
#' @return A logical vector equal in length to `vector`.
#' Equivalent to `vector %in% valid_values` if `valid_values`
#' is not `NULL` and to `\(x) { TRUE }` if
#' `valid_values` is `NULL`.
nullable_is_in <- function(vector,
                           valid_values) {
  return(
    if (!is.null(valid_values)) {
      vector %in% valid_values
    } else {
      TRUE
    }
  )
}
