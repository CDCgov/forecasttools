#' Get an approximate ordinal cumulative distribution function
#' or probability mass function from quantiles for a continuous
#' variable.
#'
#' Given quantiles that characterize a continuous random variable
#' and a set of cutpoints for binning that variable into ordinal
#' categories, return the approximate CDF or PMF for the ordinal
#' variable.
#'
#' Wraps [distfromq::make_p_fn()] to approximate the continuous
#' CDF, then applies category cutpoints in the form used by
#' [base::cut()].
#'
#' The category cutpoints must cover the entire support of
#' the continuous random variable: the first cutpoint should
#' be the lower bound of the support; the final cutpoint should
#' be the upper bound of the support.
#'
#' The function will error if the input quantile values include
#' values outside this support.
#'
#' The approximate continuous CDF will be estimated on the unconstrained
#' real interval \eqn{(-\infty, \infty)} with an appropriate monotonic
#' transform for the support, picked via [get_transform_to_real_line()].
#' It will then be evaluated against the transformed values of the
#' cutpoints.
#'
#' @param quantile_levels Vector of quantile levels, passed as
#' the `ps` argument to [distfromq::make_p_fn()].
#' @param values Vector of associated values of the random variable
#' at those quantiles, passed as the `qs` argument to
#' [distfromq::make_p_fn()].
#' @param category_cutpoints Vector (optionally named) of category
#' cutpoints that could be supplied to [base::cut()] or similar to
#' bin the continuous random variable. `forecasttools` functions such
#' as [get_prism_cutpoints()] supply cutpoints in this format.
#'
#' To follow [base::cut()] conventions, the cutpoint vector
#' should be of length \eqn{n+1}, where \eqn{n} is the number
#' of categories. Entries \eqn{1, ..., n} should be the left endpoints of
#' of each categorical bin. Entry \eqn{n+1} should be the _right_ endpoint
#' of the final bin, i.e. the upper bound of the underlying continuous
#' variable's support, or `Inf` if there is no upper bound.
#' Similarly, if there is no lower bound to the underlying variable's
#' support, entry \eqn{1} should be `-Inf`.
#' @param labels Labels for the CDF or PMF categories. For CDFs,
#' the `labels` vector should be equal in length to the cutpoints vector.
#' For PMFs, the labels vector should have one fewer entry than the
#' cutpoints vector (since the last cutpoint is the right endpoint
#' of the final bin). If no `labels` vector is specified,
#' the function uses the names of the cutpoints vector itself,
#' if any, and otherwise returns an unnamed vector. For PMFs,
#' that means all cutpoint names _except_ the last.
#' @param ... Additional keyword arguments passed to
#' [distfromq::make_p_fn()].
#' @return The values of the approximate CDF or PMF for
#' each provided category,  as a vector
#' (a named vector if the category bounds vector is named)
#'
#' @examples
#'
#' cutpoints <- get_prism_cutpoints("US", "COVID-19", signal = "NSSP")[[1]]
#' quantile_levels <- c(0.25, 0.5, 0.75)
#' values <- c(0.05, 0.2, 0.3)
#' quantiles_to_category_cdf(
#'      quantile_levels,
#'      values,
#'      cutpoints
#' )
#'
#' quantiles_to_category_pmf(
#'      quantile_levels,
#'      values,
#'      cutpoints
#' )
#'
#' @export
quantiles_to_category_cdf <- function(
  quantile_levels,
  values,
  category_cutpoints,
  labels = names(category_cutpoints),
  ...
) {
  support_lb <- min(category_cutpoints)
  support_ub <- max(category_cutpoints)
  if (
    !checkmate::test_numeric(
      values,
      lower = support_lb,
      upper = support_ub
    )
  ) {
    cli::cli_abort(c(
      paste0(
        "Got quantile values in {.arg values} outside the category ",
        "bins specified in {.arg category_cutpoints}. "
      ),
      "i" = paste0(
        "{.fn quantiles_to_category_cdf} uses the category bin endpoints ",
        "to determine the support of the random variable."
      )
    ))
  }

  if (
    !checkmate::test_character(
      labels,
      len = length(category_cutpoints),
      null.ok = TRUE
    )
  ) {
    cli::cli_abort(
      paste0(
        "{.arg labels} must be NULL or a character vector equal ",
        "in length to the {.arg category_cutpoints} vector."
      )
    )
  }

  transform <- get_transform_to_real_line(support_lb, support_ub)

  approx_cdf <- distfromq::make_p_fn(quantile_levels, transform(values), ...)

  cdf <- approx_cdf(transform(category_cutpoints))
  return(purrr::set_names(cdf, labels))
}

#' @rdname quantiles_to_category_cdf
#' @export
quantiles_to_category_pmf <- function(
  quantile_levels,
  values,
  category_cutpoints,
  labels = names(head(category_cutpoints, -1)),
  ...
) {
  if (
    !checkmate::test_character(
      labels,
      len = length(category_cutpoints) - 1,
      null.ok = TRUE
    )
  ) {
    cli::cli_abort(
      paste0(
        "{.arg labels} must be NULL or a character vector ",
        "with one fewer entry than ",
        "the {.arg category_cutpoints} vector."
      )
    )
  }

  cdf <- quantiles_to_category_cdf(
    quantile_levels,
    values,
    category_cutpoints,
    ...
  )

  p_cat <- (dplyr::lead(cdf, 1, default = NA) - cdf) |>
    utils::head(-1) |> # strip trailing NA
    purrr::set_names(labels)

  return(p_cat)
}
