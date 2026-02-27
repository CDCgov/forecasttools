test_that(
  paste0("Summarise scores with baseline agrees with manual calculation"),
  {
    summary_test_case <- function(
      to_score,
      baseline,
      compare,
      metric_to_compare,
      by,
      ...
    ) {
      scores <- scoringutils::score(to_score)
      forecast_unit <- scoringutils::get_forecast_unit(scores)
      join_key <- forecast_unit |> purrr::discard(~ . == compare)
      scaled_rel_skill_col <- glue::glue(
        "{metric_to_compare}",
        "_scaled_relative_skill"
      )
      auto <- scores |>
        summarise_scores_with_baseline(
          baseline = baseline,
          compare = compare,
          metric_to_compare = metric_to_compare,
          by = by
        )

      auto_baseline <- dplyr::filter(auto, .data[[compare]] == !!baseline)
      expect_equal(auto_baseline$mean_scores_ratio, rep(1, nrow(auto_baseline)))
      expect_equal(
        auto_baseline[[scaled_rel_skill_col]],
        rep(1, nrow(auto_baseline))
      )

      manual <- scores |>
        dplyr::filter(.data[[compare]] == !!baseline) |>
        dplyr::rename(baseline_metric = !!metric_to_compare) |>
        dplyr::select(
          tidyselect::all_of(join_key),
          "baseline_metric"
        ) |>
        dplyr::inner_join(scores, by = join_key) |>
        scoringutils::as_scores(
          metrics = c(
            scoringutils::get_metrics(scores),
            "baseline_metric"
          )
        ) |>
        scoringutils::summarise_scores(by = c(compare, by)) |>
        dplyr::mutate(
          mean_scores_ratio = .data[[metric_to_compare]] /
            .data$baseline_metric
        ) |>
        dplyr::select(-"baseline_metric") |>
        scoringutils::as_scores(
          metrics = scoringutils::get_metrics(scores)
        )

      expect_equal(
        auto |>
          dplyr::select(-tidyselect::all_of(scaled_rel_skill_col)) |>
          dplyr::arrange(
            .data[[compare]],
            dplyr::across(tidyselect::any_of(by))
          ) |>
          dplyr::select(sort(tidyselect::peek_vars())),
        dplyr::arrange(
          manual,
          .data[[compare]],
          dplyr::across(tidyselect::any_of(by))
        ) |>
          dplyr::select(sort(tidyselect::peek_vars()))
      )
    }

    summary_test_case(
      scoringutils::example_quantile,
      "EuroCOVIDhub-baseline",
      "model",
      "wis",
      NULL
    )
    summary_test_case(
      scoringutils::example_quantile,
      "EuroCOVIDhub-baseline",
      "model",
      "wis",
      "location"
    )
    summary_test_case(
      scoringutils::example_sample_discrete,
      "EuroCOVIDhub-baseline",
      "model",
      "crps",
      c("target_type", "location")
    )
    summary_test_case(
      scoringutils::example_sample_discrete |>
        dplyr::filter(
          .data$model %in% c("EuroCOVIDhub-baseline", "EuroCOVIDhub-ensemble")
        ),
      "EuroCOVIDhub-baseline",
      "model",
      "crps",
      c("target_type", "location")
    )
    summary_test_case(
      scoringutils::example_sample_discrete |>
        dplyr::rename(new_model_name = "model"),
      "EuroCOVIDhub-baseline",
      "new_model_name",
      "crps",
      "target_type"
    )
    expect_error(
      summary_test_case(
        scoringutils::example_sample_discrete |>
          dplyr::filter(.data$model == "EuroCOVIDhub-baseline"),
        "EuroCOVIDhub-baseline",
        "model",
        "crps",
        "target_type"
      ),
      "More than one non-baseline"
    )

    expect_error(
      summary_test_case(
        scoringutils::example_sample_discrete,
        "EuroCOVIDhub-baseline",
        "new_model_name",
        "crps",
        "target_type"
      ),
      "not found in data"
    )
    summary_test_case(
      scoringutils::example_sample_discrete,
      "EuroCOVIDhub-ensemble",
      "model",
      "crps",
      "target_type"
    )
  }
)


test_that("z and s spellings exist and reference the same function", {
  expect_identical(
    summarize_scores_with_baseline,
    summarise_scores_with_baseline
  )
})
