test_that("get_available_qi_widths works as expected", {
  expect_error(
    get_available_qi_widths(c(0.25, 0.5, 0.75, 1.01)),
    "<= 1"
  )
  expect_error(
    get_available_qi_widths(c(0.25, 0.5, 0.75, -0.25)),
    ">= 0"
  )

  expect_equal(
    get_available_qi_widths(c(0, 0.25, 0.3, 0.5, 0.75, 0.99)),
    0.5
  )

  expect_equal(
    get_available_qi_widths(c(0.02, 0.1, 0.2, 0.5, 0.8, 0.9, 1)),
    c(0.6, 0.8)
  )
  # works with out of order and duplicate levels
  expect_equal(
    get_available_qi_widths(c(1, 0.8, 0.8, 0.8, 0, 0.5, 0.2, 0.9, 0.1)),
    c(0.6, 0.8, 1)
  )
})


test_that("widths_to_qi_table works as expected", {
  expect_error(
    widths_to_qi_table(c(0.25, 0.5, 0.75, 1.01)),
    "<= 1"
  )
  expect_error(
    widths_to_qi_table(c(0.25, 0.5, 0.75, -0.25)),
    ">= 0"
  )

  expect_equal(
    widths_to_qi_table(c(0.5, 0.50, 0.5000)),
    tibble::tibble(
      .lower_quantile = 0.25,
      .upper_quantile = 0.75,
      .width = 0.5
    )
  )

  expect_equal(
    widths_to_qi_table(c(0, 1)),
    tibble::tibble(
      .lower_quantile = c(0.5, 0),
      .upper_quantile = c(0.5, 1),
      .width = c(0, 1)
    )
  )

  expect_equal(
    widths_to_qi_table(c(0.8, 0.8, 0.80, 0.6, 0.60, 1)),
    tibble::tibble(
      .lower_quantile = c(0.2, 0.1, 0),
      .upper_quantile = c(0.8, 0.9, 1),
      .width = c(0.6, 0.8, 1)
    )
  )
})


test_that(
  paste0(
    "quantile_table_to_median_qi throws errors ",
    "appropriately based on require_all_medians ",
    "and require_all_widths"
  ),
  {
    test_table <- tibble::tibble(
      q_level = rep(c(0.1, 0.2, 0.5, 0.8, 0.9), 3),
      q_value = 1:15,
      group = rep(c("A", "B", "C"), each = 5)
    )

    ## test require_all_medians functionality
    without_medians <- test_table |>
      dplyr::filter(q_level != 0.5 | group == "C")

    ## if require_all_medians is FALSE and medians
    ## are missing, those values should be NA
    ## and others should be the correct values
    without_valid <- quantile_table_to_median_qi(
        without_medians,
        "q_value",
        "q_level",
        .width = 0.6,
        require_all_medians = FALSE
    )
    expect_equal(without_valid$q_value, c(NA, NA, 13))

    expect_error(
        quantile_table_to_median_qi(
            without_medians,
            "q_value",
            "q_level",
            .width = 0.6
        ),
        "require_all_medians"
    )

    ## test require_all_widths functionality
    expect_no_error(quantile_table_to_median_qi(
        test_table,
        "q_value",
        "q_level",
        .width = c(0.8, 0.6),
        require_all_widths = FALSE
    ))

    expect_error(
      quantile_table_to_median_qi(
          test_table,
          "q_value",
          "q_level",
          .width = c(0.5, 0.8, 0.2)
      ),
      "require_all_widths"
    )
  }
)

test_that(paste0("hub_quantiles_to_median_qi throws error based ",
                 "on require_only_quantiles"), {
    test_table <- tibble::tibble(
      output_type_id = rep(c(0.1, 0.2, 0.5, 0.8, 0.9), 3),
      output_type = rep("quantile", 15),
      value = 1:15,
      group = rep(c("A", "B", "C"), each = 5)
    )

    ## test require_only_quantiles functionality
    with_other_output_type <- test_table |>
      dplyr::mutate(
        output_type = ifelse(
          .data$group == "C",
          "sample",
          .data$output_type
        )
        )

    expect_no_error(hub_quantiles_to_median_qi(
      with_other_output_type,
      .width = 0.6,
      require_only_quantiles = FALSE
    ))
    expect_error(
      hub_quantiles_to_median_qi(
        with_other_output_type,
        .width = 0.6
      ),
      "quantile"
    )

})

test_that("hub_quantiles_to_median_qi output matches manual expected output", {
  input <- tibble::tibble(
    output_type_id = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.1, 0.25, 0.5, 0.75, 0.9),
    output_type = rep("quantile", 10),
    value = 1:10,
    id_col_one = rep(c("A", "B"), each = 5),
    id_col_two = "a string"
  )

  expected_output <- tibble::tibble(
    id_col_one = rep(c("A", "B"), each = 2),
    id_col_two = "a string",
    value = c(3, 3, 8, 8),
    .lower = c(2, 1, 7, 6),
    .upper = c(4, 5, 9, 10),
    .width = c(0.5, 0.8, 0.5, 0.8),
    .point = "median",
    .interval = "qi"
  )

  result <- hub_quantiles_to_median_qi(
    input,
    require_only_quantiles = TRUE,
    require_all_widths = TRUE
  )

  result_explicit <- hub_quantiles_to_median_qi(
    input,
    .width = c(0.5, 0.8),
    require_only_quantiles = TRUE,
    require_all_widths = TRUE
  )

  result_filtered <- hub_quantiles_to_median_qi(
    input,
    .width = c(0.8),
    require_only_quantiles = TRUE,
    require_all_widths = TRUE
  )

  expect_equal(result, expected_output)
  expect_equal(result, result_explicit)
  expect_equal(
    result_filtered,
    dplyr::filter(
      expected_output,
      .data$.width == 0.8
    )
  )
})
