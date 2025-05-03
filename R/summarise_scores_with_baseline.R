#' Summarize a scoring table with scores relative to a baseline model.
#'
#' Light wrapper of [scoringutils::summarise_scores()] and
#' [scoringutils::get_pairwise_comparisons()] that automatically
#' adds relative metrics with a baseline.
#'
#' @param scores A scoring object containing the scoring table with
#' quantile scores.
#' @param baseline Name of the baseline, as a string. Should be
#' an element of the column defined by the `compare` argument.
#' @param compare Column name of the column that uniquely
#' identifies sets of forecasts (typically, models) to compare, as
#' a string. Passed as the `compare` argument to
#' [scoringutils::get_pairwise_comparisons()]. Default `"model"`,
#' matching the default of [scoringutils::get_pairwise_comparisons()].
#' @param metric_to_compare Name of the (absolute) metric for which to
#' compute relative values. Passed as the
#' `metric` argument to [scoringutils::get_pairwise_comparisons()].
#' By default, chooses `"wis"`, `"crps"`, or `"brier_score"`
#' if that metric is available in the input scores. This matches the
#' default in [scoringutils::get_pairwise_comparisons()].
#' @param by Columns besides the column in `compare` to group by
#' when scoring. `by` is passed directly as the `by` argument to
#' [scoringutils::get_pairwise_comparisons()].
#' `c(compare, by)` is passed as the `by` argument to
#' [scoringutils::summarise_scores()].
#' Default `NULL`.
#' @param ... additional arguments passed to
#' [scoringutils::summarise_scores()].
#'
#' @return A data frame with summarised scores for each model, including
#' relative scores where possible.
#'
#' @examples
#' quantile_summary <- scoringutils::example_quantile |>
#'   scoringutils::score() |>
#'   summarise_scores_with_baseline(
#'   baseline = "EuroCOVIDhub-baseline")
#' )
#' print(quantile_summary)
#'
#' sample_summary <- scoringutils::example_sample_discrete |>
#'   scoringutils::score() |>
#'   summarise_scores_with_baseline(
#'   baseline = "EuroCOVIDhub-baseline",
#'   by = "location")
#' print(sample_summary)
#'
#' @export
summarise_scores_with_baseline <- function(
  scores,
  baseline,
  compare = "model",
  metric_to_compare = intersect(
    c("wis", "crps", "brier_score"),
    names(scores)
  ),
  by = NULL,
  ...
) {
  # fork for 2-model comparisons
  dual <- dplyr::n_distinct(scores[[compare]]) == 2
  baseline_use <- if (dual) NULL else baseline
  scaled_rel_skill_col <- glue::glue(
    "{metric_to_compare}",
    "_scaled_relative_skill"
  )
  relative_scores <- scoringutils::get_pairwise_comparisons(
    scores = scores,
    compare = compare,
    baseline = baseline_use,
    by = by,
    metric = metric_to_compare
  ) |>
    dplyr::filter(.data$compare_against == !!baseline)

  if (dual) {
    # in 2 model comparisons, scaled rel skill is equivalent to
    # mean_scores_ratio
    relative_scores <- relative_scores |>
      dplyr::mutate(!!scaled_rel_skill_col := .data$mean_scores_ratio)
  }

  relative_scores <- relative_scores |>
    dplyr::select(
      tidyselect::all_of(c(compare, by)),
      "mean_scores_ratio",
      tidyselect::all_of(scaled_rel_skill_col)
    )

  summarised <- scores |>
    scoringutils::summarise_scores(by = c(compare, by), ...) |>
    dplyr::inner_join(relative_scores, by = c(compare, by))

  return(summarised)
}


#' @rdname summarise_scores_with_baseline
#' @export
summarize_scores_with_baseline <- summarise_scores_with_baseline
