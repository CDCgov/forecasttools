test_that("inferencedata_to_tidy_draws converts data correctly", {
  data("ex_inferencedata_dataframe")
  result <- inferencedata_to_tidy_draws(ex_inferencedata_dataframe)

  expect_setequal(colnames(result), c("group", "data"))
  expect_setequal(result$group, c("posterior", "predictions"))

  expect_equal(
    colnames(result$data[[1]]),
    c(
      ".chain",
      ".iteration",
      ".draw",
      "a",
      "b[troll_shore]",
      "b[drawn]",
      "c[surly,media]",
      "c[surly,scant_scrap]",
      "c[surly,meter]",
      "c[surly,stair]",
      "c[noble,media]",
      "c[noble,scant_scrap]",
      "c[noble,meter]",
      "c[noble,stair]",
      "c[excel_stake,media]",
      "c[excel_stake,scant_scrap]",
      "c[excel_stake,meter]",
      "c[excel_stake,stair]"
    )
  )

  expect_equal(
    colnames(result$data[[2]]),
    c(
      ".chain",
      ".iteration",
      ".draw",
      "obs[woken]",
      "obs[awash]",
      "obs[inter_ought]"
    )
  )

  expect_no_error(
    tidybayes::spread_draws(result$data[[1]], a, b[x], c[y, z])
  )
  expect_no_error(
    tidybayes::spread_draws(result$data[[2]], obs[a])
  )
})
