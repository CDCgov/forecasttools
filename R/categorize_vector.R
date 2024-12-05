#' Catgeorize entries in a numeric vector according
#' to a list of breakpoints and labels.
#'
#' Vectorized version of [base::cut()] that
#' allows each entry in the vector to use different
#' break points and labels.
#'
#' @param values Vector of values to categorize.
#' @param break_sets list of vectors of category
#' breakpoints, one for each entry of `values`,
#' or a single set of category breakpoints to use
#' for all `values`.
#' @param label_sets list of vectors of labels to associate
#' with the the corresponding breakpoints in `break_sets`,
#' one for each entry of `break_sets` or a single set of
#' labels to use for all `break_sets`.
#' @param include.lowest Passed to [base::cut()]. Default `TRUE`.
#' @param order Passed to [base::cut()]. Default `TRUE`.
#' @param right Passed to [base::cut()]. Default `TRUE`.
#' @param ... Additional keyword arguments passed to
#' [base::cut()].
#' @return A categorized vector, as vector of ordered
#' factors.
#' @export
categorize_vector <- function(values,
                              break_sets,
                              label_sets,
                              include.lowest = TRUE, # nolint
                              order = TRUE,
                              right = TRUE,
                              ...) {
  return(purrr::pmap_vec(
    list(
      x = values,
      breaks = break_sets,
      labels = label_sets,
      include.lowest = include.lowest,
      order = order,
      right = right,
      ...
    ),
    cut
  ))
}
