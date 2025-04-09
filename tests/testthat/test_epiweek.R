test_that("epiweek_start() and epiweek_end() work as expected", {
  expect_equal(epiweek_start("iSo"), 1L)
  expect_equal(epiweek_end("ISo"), 7L)
  expect_equal(epiweek_start("USa"), 7L)
  expect_equal(epiweek_end("usA"), 6L)
  expect_equal(epiweek_start("usa"), epiweek_start("mmwr"))
  expect_equal(epiweek_end("usa"), epiweek_end("mmwr"))
})
