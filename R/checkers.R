#' Assert that a vector consists only
#' of valid hubverse output types
#'
#' Wraps [checkmate::assert_names()].
#'
#' @param x vector to check
#' @return `x`, invisibly, on assertion success, or throw an error.
#'
#' @examples
#'
#' assert_hubverse_output_types(
#'   c("sample", "sample", "quantile", "pmf"))
#'
#' tryCatch(
#'   assert_hubverse_output_types(
#'     c("sample", "sample", "sample_a")
#'   ),
#'   error = \(e) print(e)
#' )
#'
#' @export
assert_hubverse_output_types <- function(x) {
  return(
    checkmate::assert_names(
      x,
      subset.of = hubverse_valid_output_types,
      .var.name = checkmate::vname(x),
      what = "Hubverse 'output_type' values"
    )
  )
}
