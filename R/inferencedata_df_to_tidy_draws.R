#' Convert InferenceData DataFrame to nested tibble of tidy_draws
#'
#' @param idata InferenceData DataFrame (the result of calling arviz.InferenceData.to_dataframe in Python)
#'
#' @return A nested tibble, with columns group and data. Each element of data is a tidy_draws data frame
#' @export
#'
#' @examples
inferencedata_to_tidy_draws <- function(idata) {
  idata |>
    dplyr::rename(
      .chain = chain,
      .iteration = draw
    ) |>
    dplyr::rename_with(function(colnames) {
      colnames |>
        stringr::str_remove_all("^\\(|\\)$") |>
        stringr::str_split(", ") |>
        purrr::map(\(x) stringr::str_remove_all(x, "^\\'|\\'$")) |>
        purrr::map(\(x) stringr::str_remove_all(x, '\\"')) |>
        purrr::map(\(x) {
          if (length(x) == 3) {
            x[2] <- stringr::str_replace(
              string = x[2],
              pattern = "(?<=\\[).+(?=\\])",
              replacement = stringr::str_replace_all(x[3], "\\s", "_")
            )
            # white space not allowed in dimension names
            x <- x[-3]
          }
          x
        }) |>
        purrr::map_chr(\(x) stringr::str_c(x, collapse = ","))
    }, .cols = -tidyselect::starts_with(".")) |>
    dplyr::mutate(dplyr::across(c(.chain, .iteration), \(x) as.integer(x + 1))) |>
    dplyr::mutate(
      .draw = tidybayes:::draw_from_chain_and_iteration_(.chain, .iteration),
      .after = .iteration
    ) |>
    tidyr::pivot_longer(-starts_with("."),
      names_sep = ",",
      names_to = c("group", "name")
    ) |>
    dplyr::group_by(group) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(data, \(x) tidyr::drop_na(x) |>
      tidyr::pivot_wider(names_from = name) |>
      tidybayes::tidy_draws()))
}
