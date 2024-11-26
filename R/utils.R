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
