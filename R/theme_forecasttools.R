#' forecasttools ggplot2 theme, built
#' on top of [ggplot2::theme_minimal()]
#' @param ... arguments passed to [ggplot2::theme()]
#' @return The theme, which can be added to a ggplot
#' object.
#' @export
theme_forecasttools <- function(...) {
  return(
    ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(...)
  )
}
