#' Convert InferenceData DataFrame to nest tidy_draws
#'
#' @param idata InferenceData DataFrame (the result of calling arviz.InferenceData.to_dataframe in Python)
#'
#' @return A nested tibble, with columns group and data. Each element of data is a tidy_draws data frame
#' @export
#'
#' @examples
inferencedata_to_tidy_draws <- function(idata) {
  idata |>
    rename(
      .chain = chain,
      .iteration = draw
    ) |>
    rename_with(function(colnames) {
      colnames |>
        str_remove_all("^\\(|\\)$") |>
        str_split(", ") |>
        map(\(x) str_remove_all(x, "^\\'|\\'$")) |>
        map(\(x) str_remove_all(x, '\\"')) |>
        map(\(x) {
          if (length(x) == 3) {
            x[2] <- str_replace(
              string = x[2],
              pattern = "(?<=\\[).+(?=\\])",
              replacement = str_replace_all(x[3], "\\s", "_")
            )
            # white space not allowed in dimension names
            x <- x[-3]
          }
          x
        }) |>
        map_chr(\(x) str_c(x, collapse = ","))
    }, .cols = -starts_with(".")) |>
    mutate(across(c(.chain, .iteration), \(x) as.integer(x + 1))) |>
    mutate(
      .draw = tidybayes:::draw_from_chain_and_iteration_(.chain, .iteration),
      .after = .iteration
    ) |>
    pivot_longer(-starts_with("."),
      names_sep = ",",
      names_to = c("group", "name")
    ) |>
    group_by(group) |>
    nest() |>
    mutate(data = map(data, \(x) drop_na(x) |>
      pivot_wider(names_from = name) |>
      tidy_draws()))
}
