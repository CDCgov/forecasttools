#' @export
#' @rdname scale_prism
prism_colors <- list(
  activity_levels = c(
    "Very Low" = "#D7F2ED",
    "Low" = "#B8E5AC",
    "Moderate" = "#FEA82F",
    "High" = "#F45B53",
    "Very High" = "#A03169"
  ),
  na_color = "#EBEBEB",
  na_label = "Data Unavailable"
)

#' @export
#' @rdname scale_prism
prism_levels <- factor(
  c(names(prism_colors$activity_levels), NA),
  levels = c(names(prism_colors$activity_levels), NA),
  ordered = TRUE,
  exclude = NULL
)


#' Prism color scale
#'
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()]
#'
#' @export
#' @rdname scale_prism
#' @examples
#' ex_dat <- data.frame(
#'   x = 1:6, y = 1:6,
#'   z = c("Very Low", "Low", "Moderate", "High", "Very High", NA)
#' )
#' # Basic plot
#' ex_dat |>
#'   ggplot2::ggplot(ggplot2::aes(x, y, color = z)) +
#'   ggplot2::geom_point(size = 5) +
#'   scale_color_prism()
#'
#' # Dropping levels drops them from the legend
#' ex_dat |>
#'   tidyr::drop_na() |>
#'   dplyr::filter(z != "Moderate") |>
#'   ggplot2::ggplot(ggplot2::aes(x, y, color = z)) +
#'   ggplot2::geom_point(size = 5) +
#'   scale_color_prism()
#'
#' # New level gets converted to NA
#' ex_dat |>
#'   tibble::add_row(x = 7, y = 7, z = "new level") |>
#'   ggplot2::ggplot(ggplot2::aes(x, y, color = z)) +
#'   ggplot2::geom_point(size = 5) +
#'   scale_color_prism()
scale_fill_prism <- function(...) {
  ggplot2::scale_fill_manual(
    values = prism_colors$activity_levels,
    na.value = prism_colors$na_color,
    breaks = levels(prism_levels),
    na.translate = TRUE,
    labels = \(x) ifelse(is.na(x), prism_colors$na_label, x),
    ...
  )
}

#' @export
#' @rdname scale_prism
scale_color_prism <- function(...) {
  ggplot2::scale_color_manual(
    values = prism_colors$activity_levels,
    na.value = prism_colors$na_color,
    breaks = levels(prism_levels),
    na.translate = TRUE,
    labels = \(x) ifelse(is.na(x), prism_colors$na_label, x),
    ...
  )
}

#' @export
#' @rdname scale_prism
scale_colour_prism <- scale_color_prism
