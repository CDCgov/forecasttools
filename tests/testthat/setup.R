library(httptest2)
mockdir <- "api_mocks"


expected_nssp_locations <- stringr::str_to_lower(
  c(datasets::state.abb, "DC", "US")
)

expected_nhsn_locations <- stringr::str_to_lower(
  c(datasets::state.abb, "DC", "PR", "VI", "US")
)

expected_prism_locations <- list(
  "nssp" = expected_nssp_locations,
  "nhsn" = expected_nhsn_locations
)
