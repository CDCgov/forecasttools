% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_prism.R
\name{scale_fill_prism}
\alias{scale_fill_prism}
\alias{scale_color_prism}
\alias{scale_colour_prism}
\title{Prism color scale}
\usage{
scale_fill_prism(...)

scale_color_prism(...)

scale_colour_prism(...)
}
\arguments{
\item{...}{Other arguments passed on to \code{\link[=discrete_scale]{discrete_scale()}}}
}
\description{
Prism color scale
}
\examples{
ex_dat <- data.frame(
  x = 1:6, y = 1:6,
  z = c("Very Low", "Low", "Moderate", "High", "Very High", NA)
)
# Basic plot
ex_dat |>
  ggplot2::ggplot(ggplot2::aes(x, y, color = z)) +
  ggplot2::geom_point(size = 5) +
  scale_color_prism()

# Dropping levels drops them from the legend
ex_dat |>
  tidyr::drop_na() |>
  dplyr::filter(z != "Moderate") |>
  ggplot2::ggplot(ggplot2::aes(x, y, color = z)) +
  ggplot2::geom_point(size = 5) +
  scale_color_prism()

# New level gets converted to NA
ex_dat |>
  tibble::add_row(x = 7, y = 7, z = "new level") |>
  ggplot2::ggplot(ggplot2::aes(x, y, color = z)) +
  ggplot2::geom_point(size = 5) +
  scale_color_prism()
}
