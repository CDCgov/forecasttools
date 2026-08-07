expect_pmf_and_cdf_consistent <- function(pmf, cdf) {
  expect_equal(
    cumsum(pmf) |> setNames(tail(names(cdf), -1)),
    # cdf named by left endpoint
    tail(cdf, -1)
  )
}


test_that(
  paste0(
    "quantiles_to_category functions work as expected ",
    "with get_prism_cutpoints() output"
  ),
  {
    cutpoints <- get_prism_cutpoints("US", "COVID-19")[[1]]

    quantile_levels <- c(0.25, 0.5, 0.75)
    values <- c(0.05, 0.2, 0.3)

    cdf <- quantiles_to_category_cdf(
      quantile_levels,
      values,
      cutpoints,
    )

    # should be a true CDF
    checkmate::expect_numeric(cdf, lower = 0, upper = 1, sorted = TRUE)
    # should preserve cutpoint names
    checkmate::expect_names(names(cdf), identical.to = names(cutpoints))

    pmf <- quantiles_to_category_pmf(
      quantile_levels,
      values,
      cutpoints
    )

    # should be proper PMF
    checkmate::expect_numeric(pmf, lower = 0, upper = 1)
    expect_pmf_and_cdf_consistent(pmf, cdf)
  }
)


test_that("support validation logic rejects invalid inputs", {
  levels <- c(0.25, 0.5, 0.75)
  values <- c(0.05, 0.2, 0.3)
  bins <- c(-2, -1.5, 0)

  expect_error(
    quantiles_to_category_cdf(
      levels,
      values,
      bins
    ),
    "outside the category bins"
  )
  expect_error(
    quantiles_to_category_pmf(
      levels,
      values,
      bins
    ),
    "outside the category bins"
  )
})

test_that("transform inference works as expected", {
  levels <- c(0.001, 0.5, 0.9)
  values <- c(5, 10, 15)
  bins <- c(-1, 1, 2.5, 10, 30)

  cdf_auto <- quantiles_to_category_cdf(
    levels,
    values,
    bins
  )

  .transform <- get_transform_to_real_line(-1, 30)
  cdf_manual <- quantiles_to_category_cdf(
    levels,
    .transform(values),
    .transform(bins)
  )

  expect_equal(
    cdf_auto,
    cdf_manual
  )

  pmf_auto <- quantiles_to_category_pmf(
    levels,
    values,
    bins
  )

  pmf_manual <- quantiles_to_category_pmf(
    levels,
    .transform(values),
    .transform(bins)
  )

  expect_equal(pmf_auto, pmf_manual)
})

test_that("name handling works as expected", {
  levels <- c(0.1, 0.5, 0.9)
  values <- c(5, 5.005, 10.8)
  named_bins <- c("low" = 4, "medium" = 4.5, "high" = 10, "upper_bound" = 20)
  unnamed_bins <- unname(named_bins)
  expect_null(names(unnamed_bins))

  cdf_named <- quantiles_to_category_cdf(
    levels,
    values,
    named_bins
  )
  cdf_unnamed <- quantiles_to_category_cdf(
    levels,
    values,
    unnamed_bins
  )
  pmf_named <- quantiles_to_category_pmf(
    levels,
    values,
    named_bins
  )
  pmf_unnamed <- quantiles_to_category_pmf(
    levels,
    values,
    unnamed_bins
  )

  expect_null(names(cdf_unnamed))
  expect_null(names(pmf_unnamed))

  ## CDF names should be left endpoints
  ## and should include the upper bound. PMF
  ## names should exclude the upper bound.
  checkmate::expect_names(names(cdf_named), identical.to = names(named_bins))
  checkmate::expect_names(
    names(pmf_named),
    identical.to = names(named_bins) |>
      head(-1)
  ) # strip trailing entry

  ## apart from names themselves, results should be
  ## same regardless of whether input bins were named
  expect_equal(unname(cdf_named), cdf_unnamed)
  expect_equal(unname(pmf_named), pmf_unnamed)
})
