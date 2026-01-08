nation <- tibble::tibble(
  code = "US",
  abbr = "US",
  hrd = "USA",
  name = "United States"
)

states <- readr::read_delim(
  "https://www2.census.gov/geo/docs/reference/state.txt",
  delim = "|"
) |>
  dplyr::select(
    code = STATE,
    abbr = STUSAB,
    hrd = STUSAB,
    name = STATE_NAME
  )

us_location_table <- dplyr::bind_rows(
  nation,
  states
)

usethis::use_data(us_location_table, overwrite = TRUE, ascii = TRUE)
