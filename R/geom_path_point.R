#' Legend key glyph for [GeomPathPoint] and subclasses of it.
#' @inheritParams ggplot2::draw_key
#' @export
draw_key_path_point <- function(data, params, size) {
  path_key <- ggplot2::draw_key_path(data, params, size)
  point_key <- ggplot2::draw_key_point(data, size, params)

  grob <- grid::grobTree(
    path_key,
    point_key
  )
  attr(grob, "width") <- attr(path_key, "width")
  attr(grob, "height") <- attr(path_key, "height")
  return(grob)
}


#' Geometric object showing a path of connected points.
#'
#' @export
  # nolint start
GeomPathPoint <- ggplot2::ggproto(
  # nolint end
  "GeomPathPoint",
  ggplot2::Geom,
  non_missing_aes = union(
    ggplot2::GeomPath$non_missing_aes,
    ggplot2::GeomPoint$non_missing_aes
  ),
  required_aes = union(
    ggplot2::GeomPath$required_aes,
    ggplot2::GeomPoint$required_aes
  ),
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    shape = 19,
    size = 1.5,
    fill = NA,
    alpha = NA,
    stroke = 0.5
  ),
  handle_na = ggplot2::GeomPath$handle_na,
  draw_group = function(
    data,
    panel_params,
    coord,
    arrow = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE, # nolint
    ...
  ) {
    path <- ggplot2::GeomPath$draw_panel(
      data,
      panel_params,
      coord,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
      )
    point <- ggplot2::GeomPoint$draw_panel(
      data,
      panel_params,
      coord,
      ...
    )
    return(grid::gList(path, point))
  },
  draw_key = draw_key_path_point
)

#' Geometric object showing a path of connected points sorted by
#' orderded in the order of the x-axis variable.
#'
#' @rdname GeomPathPoint
#' @export
  # nolint start
GeomLinePoint <- ggplot2::ggproto(
  # nolint end
  "GeomLinePoint",
  GeomPathPoint,
  setup_params = ggplot2::GeomLine$setup_params,
  extra_params = ggplot2::GeomLine$extra_params,
  setup_data = ggplot2::GeomLine$setup_data
)


#' @title Plot a path of connected points
#'
#' @description
#' Adding `geom_path_point()` or `geom_line_point()` to a
#' [`ggplot`][ggplot2::ggplot()] is similar to adding
#' [`geom_path()`][ggplot2::geom_path()] followed by
#' [`geom_point()`][ggplot2::geom_point()] or
#' [`geom_line()`][ggplot2::geom_line()] followed by
#' [`geom_point()`][ggplot2::geom_point()], respectively.
#' The difference is that that with the unified `geom_path_point()`
#' and `geom_line_point()` geoms, paths/lines and points are
#' plotted together, as a single [`layer`][ggplot2::layer_geoms].
#' In the stacked approach, they are plotted as two separate layers.
#' See Details for a discussion of why this can matter in practice.
#'
#' @details
#' Plotting with a single layer is important when the user wants
#' points and lines to be stacked ("z-ordered") _together_ by
#' [group][ggplot2::group], i.e. all points and lines representing
#' group 1 to be below all points and lines in group 2, et cetera.
#' With the standard `geom_line() + geom_point()` approach, all
#' points (z-ordered by group) will be plotted on top of all lines
#' (z-ordered by group), or the reverse if one does
#' `geom_point() + geom_line()`. this can make the plot more difficult
#' to read when points partially overlap. See the Examples for a
#' demonstration.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#'
#' @examples
#' library(ggplot2)
#' library(tibble)
#' data <- tibble(x = rep(c(3,1,2), 2),
#'                y = c(-0.5, 0.5, 0, 0.5, 0, 0.02),
#'                z = c(rep("a", 3), rep("b", 3)))
#'
#' data <- tribble(
#'    ~x,      ~y,   ~z,
#'     1,       0,  "a",
#'     2,       0,  "a",
#'     3,    -0.5,  "a",
#'     1,    0.01,  "b",
#'     3,   -.520,  "b",
#'     2,     0.02,  "b")
#'
#' # standard approach causes points to "interrupt"
#' # lines when overplotted
#' ggplot(data, aes(x = x, y = y, color = z)) +
#'    geom_path(linewidth = 2) +
#'    geom_point(size = 5)
#'
#' # using a single-layer approach fixes this
#' ggplot(data, aes(x = x, y = y, color = z)) +
#'    geom_path_point(linewidth = 2, size = 5)
#'
#' # The same is true for `geom_line` and `geom_line_point`
#' # multi-layer approach:
#' ggplot(data, aes(x = x, y = y, color = z)) +
#'    geom_line(linewidth = 3) +
#'    geom_point(size = 5)
#'
#' # single-layer approach:
#' ggplot(data, aes(x = x, y = y, color = z)) +
#'    geom_line_point(linewidth = 3, size = 5)
#'
#' @export
geom_path_point <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = NULL,
  na.rm = FALSE, # nolint
  show.legend = NA, # nolint
  inherit.aes = TRUE, # nolint
  ...
) {
  return(ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPathPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  ))
}

#' @rdname geom_path_point
#' @export
geom_line_point <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE, # nolint
  orientation = NA,
  show.legend = NA, # nolint
  inherit.aes = TRUE, # nolint
  ...
) {
  return(ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLinePoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  ))
}
