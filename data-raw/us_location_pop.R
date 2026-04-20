get_latest_population <- function(geography) {
  for (v in seq(
    lubridate::year(lubridate::today()),
    by = -1,
    length.out = 10
  )) {
    x <- tryCatch(
      tidycensus::get_estimates(
        geography = geography,
        product = "population",
        vintage = v,
        year = v
      ),
      error = function(e) NULL
    )
    if (!is.null(x)) {
      return(
        x |>
          dplyr::filter(.data$variable == "POPESTIMATE") |>
          dplyr::rename_with(stringr::str_to_lower) |>
          dplyr::select("name", population = "value") |>
          dplyr::muate(population = as.integer(.data$population))
      )
    }
  }
  stop("No PEP vintage available in tested range.")
}

us_location_pop <- dplyr::bind_rows(
  get_latest_population("us"),
  get_latest_population("state")
)

usethis::use_data(us_location_pop, overwrite = TRUE, ascii = TRUE)
