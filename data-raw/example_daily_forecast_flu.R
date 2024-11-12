example_daily_forecast_flu <- readr::read_tsv(fs::path(
  "inst",
  "extdata",
  "example_daily_forecast_flu",
  ext = "tsv"
))

usethis::use_data(example_daily_forecast_flu, overwrite = TRUE)
