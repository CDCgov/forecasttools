#' Validate the base forecasts
#'
#' Validate the base forecasts tibble. This is aimed at, first, checking
#' that columns exist as expected and, second, that it is possible to sample
#' trajectories from the base forecast tibble by matching a draw index for each
#' location along an ascending sequence of dates.
#' @param base_forecasts The base forecasts tibble
#' @param cp Base forecast sampling copula. This expects
#' a copula object from the `copula` package, for example
#' `copula::normalCopula(0.5, dim = 53)`.
#' @param draw_col String name of column for draws.
#' @param date_col String name of column for dates.
#' @param value_to_aggregate_col String name of column to make bottom-up
#' aggregation on.
#' @param rank_quantity_col String name of column to rank draw/location
#' pair on.
#' @param location_col String name of the location column.
#'
#' @return Nothing.
#' @export
validate_base_forecasts <- function(
  base_forecasts,
  cp,
  draw_col = ".draw",
  date_col = "date",
  value_to_aggregate_col = "hosp",
  rank_quantity_col = "hosp",
  location_col = "location"
) {
  if (!tibble::is_tibble(base_forecasts)) {
    cli::cli_abort("Base forecasts are not in tibble format.")
  }
  if (!(draw_col %in% colnames(base_forecasts))) {
    cli::cli_abort(
      "The draw column {draw_col} does not exist
                   for base forecasts."
    )
  }
  if (!(date_col %in% colnames(base_forecasts))) {
    cli::cli_abort(
      "The date column {date_col} does not exist
                   for base forecasts."
    )
  }
  if (!(value_to_aggregate_col %in% colnames(base_forecasts))) {
    cli::cli_abort(
      "The aggregation column {value_to_aggregate_col}
    does not exist for base forecasts."
    )
  }
  if (!(rank_quantity_col %in% colnames(base_forecasts))) {
    cli::cli_abort(
      "The ranking quantity column {rank_quantity_col}
    does not exist for base forecasts."
    )
  }
  if (!(location_col %in% colnames(base_forecasts))) {
    cli::cli_abort(
      "The location column {location_col} does not exist for base
                   forecasts."
    )
  }
  if (cp@dimension != length(unique(base_forecasts[[location_col]]))) {
    cli::cli_abort(
      "The copula dimension and the number of base forecast
                   locations don't match"
    )
  }
  counted_samples <- count_trajectories(
    base_forecasts,
    location_col,
    date_col
  )
  fc1 <- length(counted_samples[[location_col]]) !=
    length(unique(base_forecasts[[location_col]]))
  fc2 <- !setequal(
    counted_samples[[location_col]],
    base_forecasts[[location_col]]
  )
  if (fc1) {
    cli::cli_abort(
      "The number of sampled trajectories per distinct values
        in column {location_col} can't be counted. Possibly due to different
        numbers of samples per forecast per date across at least one
        location."
    )
  }
  if (fc2) {
    cli::cli_abort(
      "The distinct values in column {location_col} differ
    between the base forecasts and the counted trajectories"
    )
  }
}

#' Count the number of trajectories per location
#'
#' This function takes a base forecast dataset and counts the number of
#' trajectories per location. In this context a trajectory is a sequence
#' of forecast values (i.e. rows in base forecast) with the same
#' draw id and location along with ascending date values.
#'
#' @param base_forecasts The base forecast dataset.
#' @param location_col The name of the column containing the location
#' information.
#' @param date_col The name of the column containing the date information.
#'
#' @return A tibble with the number of samples per forecast, grouped
#' by location.
#' @export
count_trajectories <- function(base_forecasts, location_col, date_col) {
  number_of_sampled_trajs <- base_forecasts |>
    dplyr::group_by(.data[[location_col]], .data[[date_col]]) |>
    dplyr::summarise(n_sample_trajs = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::distinct(.data[[location_col]], .data$n_sample_trajs)
  return(number_of_sampled_trajs)
}

#' Rank sampled trajectories
#'
#' This function ranks the base forecasts based on a specified quantity,
#' recorded in the `rank_quantity_col` column.
#' grouped by location and draw.
#'
#' @param base_forecasts A data frame containing the base forecasts.
#' @param location_col The name of the column representing the location.
#' @param draw_col The name of the column representing the draw.
#' @param rank_quantity_col The name of the column representing the quantity
#' to rank trajectories by.
#'
#' @return A data frame with the ranked base forecasts.
#' @export
rank_sampled_trajectories <- function(
  base_forecasts,
  location_col,
  draw_col,
  rank_quantity_col
) {
  ranked_base_forecasts <- base_forecasts |>
    dplyr::group_by(.data[[draw_col]], .data[[location_col]]) |>
    dplyr::summarise(mean_rank_quantity = mean(.data[[rank_quantity_col]])) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data[[location_col]]) |>
    dplyr::mutate(
      rank = rank(.data$mean_rank_quantity, ties.method = "first")
    ) |>
    dplyr::arrange(.data[[location_col]], rank) |>
    dplyr::ungroup()
  return(ranked_base_forecasts)
}


#' Convert Copula Samples to a Tibble
#'
#' This function converts a matrix of copula samples to a tibble format.
#'
#' @param i The index of the sample.
#' @param u_mat The matrix of Copula samples.
#' @param location_names The names of the locations.
#' @param location_col String name of the location column.
#'
#' @return A tibble containing the Copula samples and location names.
#' @export
copula2tbl <- function(i, u_mat, location_names, location_col) {
  samples <- dplyr::tibble(u = u_mat[i, location_names])
  samples[[location_col]] <- location_names
  return(samples)
}

#' Generate sample trajectories based on ranked base forecasts
#'
#' This function takes in a set of copula samples, ranked base forecasts,
#' and column names to generate sampled trajectories for each location and then
#' sum aggregate by date over locations.
#'
#' @param base_forecasts A data frame containing the base forecasts.
#' @param samples A tibble containing the samples from a copula by
#' location.
#' @param ranked_base_forecasts A tibble containing the ranked base forecasts
#' @param number_of_sampled_trajs A tibble containing the number of
#' samples per forecast trajectory for each location.
#' @param location_col The name of the column representing the location
#' @param draw_col The name of the column representing the draw
#' @param date_col The name of the column representing the date
#' @param value_to_aggregate_col The name of the column representing
#' the value to aggregate
#'
#' @return A data frame containing the sampled forecasts
#' @export
sample_aggregated_trajectories <- function(
  base_forecasts,
  samples,
  ranked_base_forecasts,
  number_of_sampled_trajs,
  location_col,
  draw_col,
  date_col,
  value_to_aggregate_col
) {
  sampled_forecasts <- samples |>
    dplyr::left_join(number_of_sampled_trajs, by = location_col) |>
    dplyr::mutate(rank_draw = ceiling(.data$u * .data$n_sample_trajs)) |>
    dplyr::select("rank_draw", dplyr::all_of(location_col)) |>
    dplyr::right_join(ranked_base_forecasts, by = location_col) |>
    dplyr::group_by(.data[[location_col]]) |>
    dplyr::slice(dplyr::first(.data$rank_draw)) |>
    dplyr::ungroup() |>
    dplyr::inner_join(base_forecasts, by = c(draw_col, location_col)) |>
    dplyr::group_by(.data[[date_col]]) |>
    dplyr::summarise(forecast = sum(.data[[value_to_aggregate_col]]))

  return(sampled_forecasts)
}

#' Bottom up aggregation of base forecasts
#'
#' @param base_forecasts The base forecasts tibble
#' @param cp Base forecast rank sampling copula.
#' @param ndraws Number of draws from rank sampling copula
#' @param draw_col String name of column for draws.
#' @param date_col String name of column for dates.
#' @param value_to_aggregate_col String name of column to make bottom-up
#' aggregation on.
#' @param rank_quantity_col String name of column to rank draw/location
#' pair on.
#' @param location_col String name of the location column.
#' @param aggregated_location_name Name for the aggregated location.
#' Default `"aggregate"`.
#' @return A tibble of 1,..., `ndraws` aggregated forecasts.
#' @export
bottom_up_aggregation <- function(
  base_forecasts,
  cp,
  ndraws,
  draw_col = ".draw",
  date_col = "date",
  value_to_aggregate_col = "hosp",
  rank_quantity_col = "hosp",
  location_col = "location",
  aggregated_location_name = "aggregate"
) {
  validate_base_forecasts(
    base_forecasts,
    cp,
    draw_col = draw_col,
    date_col = date_col,
    value_to_aggregate_col = value_to_aggregate_col,
    rank_quantity_col = rank_quantity_col,
    location_col = location_col
  )

  number_of_sampled_trajs <- count_trajectories(
    base_forecasts,
    location_col,
    date_col
  )
  ranked_base_forecasts <- rank_sampled_trajectories(
    base_forecasts,
    location_col,
    draw_col,
    rank_quantity_col
  )

  location_names <- unique(base_forecasts[[location_col]])
  u_mat <- copula::rCopula(ndraws, cp)
  colnames(u_mat) <- location_names

  aggregated_forecast <- lapply(
    1:ndraws,
    function(i) {
      samples <- copula2tbl(i, u_mat, location_names, location_col)

      sample_trajectory <-
        sample_aggregated_trajectories(
          base_forecasts,
          samples,
          ranked_base_forecasts,
          number_of_sampled_trajs,
          location_col,
          draw_col,
          date_col,
          value_to_aggregate_col
        )
      sample_trajectory[[draw_col]] <- i
      sample_trajectory[[location_col]] <- aggregated_location_name
      return(sample_trajectory)
    }
  ) |>
    dplyr::bind_rows()
  return(aggregated_forecast)
}
