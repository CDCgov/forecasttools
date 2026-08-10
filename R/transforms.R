#' Scaled logit transform
#'
#' Compute \deqn{f(x) = \log(x - a) - \log(b - x)}
#' for \eqn{b > a}. This is an increasing function that
#' maps the interval \eqn{(a, b)} to \eqn{(-\infty, \infty)}.
#'
#' @param x Value to transform
#' @param a Left endpoint of the domain
#' @param b Right endpoint of the domain
#' @return Transformed value
#' @examples
#'
#' # with a = 0, b = 1, equivalent to `qlogis`
#' scaled_logit(0.7, 0, 1)
#' qlogis(0.7)
#'
#' # but can work with other domains
#' scaled_logit(3, 1, 7)
#'
#' @export
scaled_logit <- function(x, a, b) {
  return(log(x - a) - log(b - x))
}


#' Scaled and shifted logarithmic transform
#'
#' Compute \deqn{f(x) = m \log(m(x - b))}
#'
#' This is an increasing function.
#' When \eqn{m > 0}, it maps \eqn{(b, \infty)} to
#' \eqn{(-\infty, \infty)}. When \eqn{m < 0}, it maps
#' \eqn{(-\infty, b)} to \eqn{-\infty, \infty)}.
#'
#' @param x Value to transform
#' @param m Scale parameter. Most relevant for its
#' sign. Typical choices are `-1` or `1`.
#' @param b finite endpoint of the domain (whether
#' it is the left or right endpoint is determined
#' by the sign of `m`).
#' @return Transformed value
#'
#' @examples
#'
#' # with m = 1, b = 0, equivalent to `log`
#'
#' scaled_shifted_log(5, 1, 0)
#' log(5)
#'
#' # but can handle other domains
#'
#' scaled_shifted_log(-exp(3), -1, 5)
#'
#' @export
scaled_shifted_log <- function(x, m, b) {
  return(m * log(m * (x - b)))
}

#' Get a monotonic transform from a real interval
#' to the unconstrained real line.
#'
#' Uses [scaled_shifted_log()] for intervals of the form
#' \eqn{(-\infty, x]} or \eqn{[x, \infty]} with finite \eqn{x}.
#' Uses [scaled_logit()] for intervals of
#' the form \eqn{(x, y)} with finite \eqn{x, y}. Uses the identity
#' transform ([base::identity()]) for the real line
#' \eqn{(\-infty, infty)}.
#'
#' @param lb Lower bound of the interval. `-Inf` for
#' no lower bound.
#' @param ub Upper bound of the interval. `Inf` for
#' no upper bound.
#' @return The transform, as a callable object.
#'
#' @examples
#'
#' my_scaled_logit <- get_transform_to_real_line(-100, 100)
#' my_scaled_logit(0)
#' my_scaled_logit(50)
#'
#' my_shifted_log <- get_transform_to_real_line(6, Inf)
#' my_shifted_log(20)
#'
#' my_other_shifted_log <- get_transform_to_real_line(-Inf, 10)
#' my_other_shifted_log(-70)
#'
#' # returns the identity transform if given no bounds
#' my_transform <- get_transform_to_real_line(-Inf, Inf)
#' my_transform(50)
#'
#' @export
get_transform_to_real_line <- function(lb, ub) {
  checkmate::assert_number(lb)
  checkmate::assert_number(ub)
  if (ub <= lb) {
    cli::cli_abort(c(
      "{.arg ub} must be strictly greater than {.arg lb}.",
      "i" = "Got {.arg ub} = {ub} and {.arg lb} = {lb}."
    ))
  }
  if (is.infinite(lb) && is.infinite(ub)) {
    return(identity)
  } else if (is.finite(lb) && is.finite(ub)) {
    return(purrr::partial(scaled_logit, a = lb, b = ub))
  } else if (is.finite(lb) && is.infinite(ub)) {
    return(purrr::partial(scaled_shifted_log, m = 1, b = lb))
  } else if (is.infinite(lb) && is.finite(ub)) {
    return(purrr::partial(scaled_shifted_log, m = -1, b = ub))
  }
  cli::cli_abort("Unexpected error. This point should not be reached.") # nocov
}
