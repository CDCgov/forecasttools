# Simple example base forecast tbl
test_base_forecasts <- dplyr::tibble(
  location = rep(c("A", "B", "A", "B", "C", "C"), 2),
  date = c(rep("2023-03-01", 6), rep("2023-03-02", 6)),
  .draw = rep(c(1, 1, 2, 2, 1, 2), 2),
  hosps = c(
    120, 80, 40, 120, 1, 3,
    150, 110, 60, 170, 10, 30
  ),
  rank_quantity = c(
    12, 8, 4, 12, 0.1, 0.3,
    15, 11, 6, 17, 1, 3
  )
)
test_cp <- copula::normalCopula(0.5, dim = 3)


# Add an incompatible forecast which can't be collapsed to
# sampling trajectories
bad_test_base_forecasts <- test_base_forecasts |>
  dplyr::add_row(
    location = "C",
    date = "2023-03-02",
    .draw = 3,
    hosps = 1,
    rank_quantity = 0.1
  )


# Fix an example possible copula sample
test_u_mat <- matrix(c(0.1, 0.3, 0.7), nrow = 1)
location_names <- c("A", "B", "C")
colnames(test_u_mat) <- location_names

test_that("`validate_base_forecasts` throws an error", {
  expect_error(validate_base_forecasts(bad_test_base_forecasts,
    test_cp,
    value_to_aggregate_col = "hosps",
    rank_quantity_col = "rank_quantity",
    location_col = "location",
    date_col = "date"
  ))
})

test_that("`count_trajectories` returns correct number of trajs", {
  result <- count_trajectories(test_base_forecasts,
    location_col = "location",
    date_col = "date"
  )
  expected_output <- dplyr::tibble(
    location = c("A", "B", "C"),
    n_sample_trajs = c(2, 2, 2)
  )
  expect_equal(result, expected_output)
})

test_that("`copula2tbl` returns the expected output", {
  i <- 1
  location_col <- "location"

  result <- copula2tbl(i, test_u_mat, location_names, location_col)

  expected_output <- dplyr::tibble(
    u = test_u_mat[i, location_names],
    location = location_names
  )

  expect_equal(result, expected_output)
})

test_that("`rank_sampled_trajectories` returns correct rankings", {
  ranked_base_forecasts <-
    rank_sampled_trajectories(test_base_forecasts,
      location_col = "location",
      draw_col = ".draw",
      rank_quantity_col = "rank_quantity"
    )

  expected_rankings <- dplyr::tibble(
    .draw = c(2, 1, 1, 2, 1, 2),
    location = c("A", "A", "B", "B", "C", "C"),
    mean_rank_quantity = c(5, 13.5, 9.5, 14.5, 0.55, 1.65),
    rank = c(1, 2, 1, 2, 1, 2)
  )

  expect_equal(ranked_base_forecasts, expected_rankings)
})

test_that("sample_aggregated_trajectories", {
  i <- 1
  location_col <- "location"
  test_sample <- copula2tbl(
    i,
    test_u_mat,
    location_names,
    location_col
  )
  test_ranked_base_forecasts <-
    rank_sampled_trajectories(test_base_forecasts,
      location_col = "location",
      draw_col = ".draw",
      rank_quantity_col = "rank_quantity"
    )
  test_num_samples <- count_trajectories(test_base_forecasts,
    location_col = "location",
    date_col = "date"
  )

  result <- sample_aggregated_trajectories(test_base_forecasts,
    test_sample,
    test_ranked_base_forecasts,
    test_num_samples,
    location_col = "location",
    draw_col = ".draw",
    date_col = "date",
    value_to_aggregate_col =
      "hosps"
  )

  expected_output <- dplyr::tibble(
    date =
      c("2023-03-01", "2023-03-02"),
    forecast = c(123, 200)
  )

  expect_equal(result, expected_output)
})
