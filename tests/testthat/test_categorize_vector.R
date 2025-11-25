n_categories <- 10
n_cutpoints <- n_categories + 1
n_to_categorize <- 100

withr::with_seed(5, {
  to_categorize <- runif(n_to_categorize, -100, 100)
  cutpoints <- runif(n_to_categorize * n_cutpoints, -200, 200) |>
    array(dim = c(n_to_categorize, n_cutpoints)) |>
    apply(1, sort) |>
    t()
  ## need lower and upper bound cutpoints in addition to one
  ## for each label
  lab_no <- sample.int(
    5 * n_to_categorize * n_categories,
    n_to_categorize * n_categories,
    replace = FALSE
  )
})

labs <- glue::glue("L_{lab_no}") |>
  array(dim = c(n_to_categorize, n_categories))

testthat::test_that("test data has appropriate dimensions", {
  expect_equal(dim(cutpoints), c(n_to_categorize, n_cutpoints))
  expect_equal(dim(labs), c(n_to_categorize, n_categories))
  expect_equal(dim(array(to_categorize)), n_to_categorize)
})


testthat::test_that("categorize_vector() argument checking works", {
  ## this works
  expect_no_error(categorize_vector(
    to_categorize,
    list(cutpoints[1, ]),
    list(labs[1, ])
  ))

  ## these error even though the entries are the right
  ## shapes and types because one or the other is
  ## not wrapped into a length-1 list
  expect_error(categorize_vector(
    to_categorize,
    list(cutpoints[1, ]),
    labs[1, ]
  ))
  expect_error(categorize_vector(
    to_categorize,
    cutpoints[1, ],
    list(labs[1, ])
  ))

  ## these errors because a dimension for breaks or
  ## labels is neither 1 nor the length of to_categorize
  expect_error(categorize_vector(
    to_categorize,
    list(
      cutpoints[1, ],
      cutpoints[2, ]
    ),
    list(labs[1, ])
  ))
  expect_error(categorize_vector(
    to_categorize,
    list(
      cutpoints[1, ]
    ),
    list(
      labs[1, ],
      labs[2, ]
    )
  ))

  ## this errors because the labels are not a list of
  ## character vectors
  expect_error(categorize_vector(
    to_categorize,
    list(cutpoints[1, ]),
    list(a = rep(1L, length(labs[1, ])))
  ))

  ## this errors because the cutpoints are not numeric
  expect_error(categorize_vector(
    to_categorize,
    list(as.character(cutpoints[1, ])),
    list(labs[1, ])
  ))
})

testthat::test_that(
  paste0(
    "categorize_vector() is equivalent to cut() ",
    "for a single set of bins and cutpoints"
  ),
  {
    expected <- cut(
      to_categorize,
      breaks = cutpoints[1, ],
      labels = labs[1, ],
      include.lowest = TRUE,
      order = TRUE,
      right = TRUE
    )

    result <- categorize_vector(
      to_categorize,
      list(cutpoints[1, ]),
      list(labs[1, ])
    )
    expect_equal(expected, result)
  }
)

testthat::test_that(
  paste0(
    "categorize_vector() is equivalent to iterated ",
    "cut() with paired sets of cutpoints ",
    "and labels"
  ),
  {
    expected_vec_both <- replicate(n_to_categorize, NA, simplify = FALSE)
    for (i in 1:n_to_categorize) {
      expected_vec_both[[i]] <- cut(
        to_categorize[i],
        breaks = cutpoints[i, ],
        labels = labs[i, ],
        include.lowest = TRUE,
        order = FALSE,
        right = TRUE
      )
    }
    result_vec_both <- categorize_vector(
      to_categorize,
      purrr::array_branch(cutpoints, 1),
      purrr::array_branch(labs, 1),
      order = FALSE
    )

    expect_equal(
      unlist(expected_vec_both),
      result_vec_both
    )
  }
)

testthat::test_that(
  paste0(
    "categorize_vector() is equivalent to iterated ",
    "cut() with paired sets of cutpoints ",
    "and labels, and can produce ordered factors ",
    "provided all label sets are the same."
  ),
  {
    expected_vec_both <- replicate(n_to_categorize, NA, simplify = FALSE)
    for (i in 1:n_to_categorize) {
      expected_vec_both[[i]] <- cut(
        to_categorize[i],
        breaks = cutpoints[i, ],
        labels = labs[1, ],
        include.lowest = TRUE,
        order = TRUE,
        right = TRUE
      )
    }
    result_vec_both <- categorize_vector(
      to_categorize,
      purrr::array_branch(cutpoints, 1),
      replicate(n_to_categorize, labs[1, ], simplify = FALSE)
    )

    expect_equal(
      factor(unlist(expected_vec_both), ordered = TRUE),
      result_vec_both
    )
  }
)


testthat::test_that(
  paste0(
    "categorize_vector() is equivalent to iterated ",
    "cut() with vectorized cutpoints and single ",
    "sets of labels, or the reverse."
  ),
  {
    expected_vec_bins <- replicate(n_to_categorize, NA, simplify = FALSE)
    expected_vec_labs <- replicate(n_to_categorize, NA, simplify = FALSE)

    for (i in 1:n_to_categorize) {
      expected_vec_bins[[i]] <- cut(
        to_categorize[i],
        breaks = cutpoints[i, ],
        labels = labs[25, ],
        include.lowest = TRUE,
        order = TRUE,
        right = TRUE
      )
      expected_vec_labs[[i]] <- cut(
        to_categorize[i],
        breaks = cutpoints[7, ],
        labels = labs[i, ],
        include.lowest = TRUE,
        order = FALSE,
        right = TRUE
      )
    }
    result_vec_labs <- categorize_vector(
      to_categorize,
      list(cutpoints[7, ]),
      purrr::array_branch(labs, 1),
      order = FALSE
    )

    result_vec_bins <- categorize_vector(
      to_categorize,
      purrr::array_branch(cutpoints, 1),
      list(labs[25, ])
    )

    expect_equal(
      factor(unlist(expected_vec_bins), ordered = TRUE),
      result_vec_bins
    )
    expect_equal(
      unlist(expected_vec_labs),
      result_vec_labs
    )
  }
)
