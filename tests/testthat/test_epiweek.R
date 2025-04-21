test_that("epiweek_start() and epiweek_end() work as expected", {
  expect_equal(epiweek_start("iSo"), 1L)
  expect_equal(epiweek_end("ISo"), 7L)
  expect_equal(epiweek_start("USa"), 7L)
  expect_equal(epiweek_end("usA"), 6L)
  expect_equal(epiweek_start("usa"), epiweek_start("mmwr"))
  expect_equal(epiweek_end("usa"), epiweek_end("mmwr"))
})


test_that(
  paste0(
    "epiyear_n_days and epiyear_n_days results correspond ",
    "to manual expectation"
  ),
  {
    years <- 2020:2032
    expected_weeks_mmwr <- c(
      53,
      52,
      52,
      52,
      52,
      53,
      52,
      52,
      52,
      52,
      52,
      53,
      52
    )
    expected_weeks_iso <- c(
      53,
      52,
      52,
      52,
      52,
      52,
      53,
      52,
      52,
      52,
      52,
      52,
      53
    )
    expect_equal(
      epiyear_n_days(years, "usa"),
      7L * expected_weeks_mmwr
    )
    expect_equal(
      epiyear_n_weeks(years, epiweek_standard = "mMwR"),
      expected_weeks_mmwr
    )
    expect_equal(
      epiyear_n_weeks(years, epiweek_standard = "USa"),
      expected_weeks_mmwr
    )
    expect_equal(
      epiyear_n_weeks(years, epiweek_standard = "Iso"),
      expected_weeks_iso
    )
    expect_equal(
      epiyear_n_days(years, epiweek_standard = "isO"),
      7L * expected_weeks_iso
    )
  }
)


test_that("assert_date_in_epiweek() behaves as expected", {
  expect_no_warning(
    assert_date_in_epiweek("2025-01-02", 1, 2025, "USA")
  )
  expect_no_warning(assert_date_in_epiweek(
    c("2024-12-27", "2025-01-05"),
    c(52, 1),
    c(2024, 2025),
    "ISO"
  ))
  expect_no_warning(assert_date_in_epiweek(
    c("2024-12-27", "2025-01-05"),
    c(52, 1),
    c(2024, 2025),
    "isO"
  ))
  expect_error(
    assert_date_in_epiweek(
      c(
        "2024-12-01",
        "2024-12-27"
      ),
      52,
      2024,
      "ISO"
    ),
    "'2024-12-01' failed"
  )
  expect_error(
    assert_date_in_epiweek(
      c(
        "2024-12-01",
        "2025-12-27"
      ),
      52,
      2024,
      "ISO"
    ),
    "'2024-12-01 and 2025-12-27' failed"
  )
  expect_error(
    assert_date_in_epiweek(
      c(
        "2024-12-01",
        "2025-12-27",
        "2026-01-01"
      ),
      52,
      2024,
      "ISO"
    ),
    "'2024-12-01, 2025-12-27, and 2026-01-01' failed"
  )
  expect_error(
    assert_date_in_epiweek(
      c(
        "2024-12-01",
        "2025-12-27",
        "2026-01-01"
      ),
      c(52, 52, 53),
      c(2024, 2024, 2025),
      "mmwr"
    ),
    "'2024-12-01 and 2025-12-27' failed"
  )
  expect_error(
    assert_date_in_epiweek(
      c(
        "2024-12-01",
        "2025-12-27",
        "2026-01-01"
      ),
      c(52, 52, 53),
      c(2024, 2024, 2025),
      "iso"
    ),
    "'2024-12-01, 2025-12-27, and 2026-01-01' failed"
  )
})
