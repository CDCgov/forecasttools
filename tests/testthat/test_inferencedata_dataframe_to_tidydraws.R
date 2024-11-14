testthat::test_that("inferencedata_to_tidy_draws converts data correctly", {
  data("ex_inferencedata_dataframe")
  result <- inferencedata_to_tidy_draws(ex_inferencedata_dataframe)

  testthat::expect_setequal(colnames(result), c("group", "data"))
  testthat::expect_setequal(result$group, c("posterior", "predictions"))

  testthat::expect_equal(
    colnames(result$data[[1]]),
    c(
      ".chain", ".iteration", ".draw", "a", "b[troll_shore]", "b[drawn]",
      "c[surly,media]", "c[surly,scant_scrap]", "c[surly,meter]",
      "c[surly,stair]", "c[noble,media]", "c[noble,scant_scrap]",
      "c[noble,meter]", "c[noble,stair]", "c[excel_stake,media]",
      "c[excel_stake,scant_scrap]", "c[excel_stake,meter]",
      "c[excel_stake,stair]"
    )
  )

  testthat::expect_equal(
    colnames(result$data[[2]]),
    c(
      ".chain", ".iteration", ".draw", "obs[woken]", "obs[awash]",
      "obs[inter_ought]"
    )
  )

  testthat::expect_no_error(spread_draws(result$data[[1]], a, b[x], c[y, z]))
  testthat::expect_no_error(spread_draws(result$data[[2]], obs[a]))
})
