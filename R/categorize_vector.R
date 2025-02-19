#' Categorize entries in a numeric vector according
#' to a list of breakpoints and labels.
#'
#' Vectorized version of [base::cut()] that
#' allows each entry in the vector to use different
#' break points and labels.
#'
#' @param values Vector of values to categorize.
#' @param break_sets sets of category breakpoints,
#' with one set for each entry of `values`, as a list of
#' numeric vectors. Alternatively, a single set of category
#' breakpoints to use for all values, as a one-entry list
#' containing a single numeric vector.
#' @param label_sets sets of labels to associate,
#' with the corresponding breakpoints in `break_sets`,
#' with one set for each entry of `values`, as a list of
#' character vectors. Alternatively, a single set of labels
#' to use for all values, as a one-entry list containing a
#' single character vector.
#' @param include.lowest Passed to [base::cut()]. Default `TRUE`.
#' @param order Should the output factors be ordered factors?
#' Passed to [base::cut()]. Default `TRUE`.
#' @param right Passed to [base::cut()]. Default `TRUE`.
#' @param ... Additional keyword arguments passed to
#' [base::cut()].
#' @return A categorized vector, as vector of
#' factors (default ordered factors).
#' @export
categorize_vector <- function(values,
                              break_sets,
                              label_sets,
                              include.lowest = TRUE, # nolint
                              order = TRUE,
                              right = TRUE,
                              ...) {
  n_vals <- length(values)

  checkmate::assert_list(break_sets,
    types = "numeric"
  )
  checkmate::assert_list(label_sets,
    types = "character"
  )
  checkmate::qassert(break_sets, c("l1", glue::glue("l{n_vals}")))
  checkmate::qassert(label_sets, c("l1", glue::glue("l{n_vals}")))

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
