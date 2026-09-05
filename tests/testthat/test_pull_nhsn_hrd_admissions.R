test_that("nhsn_hrd_admissions_column_names pairs counts with rates", {
  purrr::walk(c("covid", "flu", "rsv"), \(disease) {
    columns <- nhsn_hrd_admissions_column_names(disease)

    checkmate::expect_names(names(columns), permutation.of = c("count", "rate"))

    expect_identical(
      columns[["rate"]],
      paste0(columns[["count"]], "per100k")
    )
  })
})


test_that("nhsn_hrd_admissions_column_names distinguishes diseases", {
  columns <- purrr::map_chr(
    c("covid", "flu", "rsv"),
    \(disease) nhsn_hrd_admissions_column_names(disease)[["count"]]
  )
  expect_length(unique(columns), 3)
})


test_that("nhsn_hrd_admissions_column_names rejects unknown diseases", {
  expect_error(
    nhsn_hrd_admissions_column_names("ari"),
    "disease"
  )
})


test_that("pull_nhsn_hrd_admissions rejects unknown HRD releases", {
  expect_error(
    pull_nhsn_hrd_admissions("covid", dataset = "nssp_prop_ed_visits"),
    "dataset"
  )
})
