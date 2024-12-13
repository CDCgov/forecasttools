#' Save a list of ggplot plots as a PDF, with a
#' grid of `nrow` by `ncol` plots per page
#'
#' @param list_of_plots List of plots to save to PDF.
#' @param save_path Path to which to save the plots. Must
#' end in `.pdf`.
#' @param nrow Number of rows of plots per page
#' (passed to [gridExtra::marrangeGrob()])
#' Default `1`.
#' @param ncol Number of columns of plots per page
#' (passed to [gridExtra::marrangeGrob()]).
#' Default `1`.
#' @param width page width in device units (passed to
#' [ggplot2::ggsave()]). Default `8.5`.
#' @param height page height in device units (passed to
#' [ggplot2::ggsave()]). Default `11`.
#' @return Nothing, saving the plots as a side effect.
#' @export
plots_to_pdf <- function(list_of_plots,
                         save_path,
                         nrow = 1,
                         ncol = 1,
                         width = 8.5,
                         height = 11) {
  save_ext <- fs::path_ext(save_path)
  if (!save_ext == "pdf") {
    cli::cli_abort(paste0(
      "{save_path} file extension must ",
      "be 'pdf', got '{save_ext}'"
    ))
  }
  cli::cli_inform("Saving plots to {save_path}")
  ggplot2::ggsave(
    filename = save_path,
    plot = gridExtra::marrangeGrob(list_of_plots,
      nrow = nrow,
      ncol = ncol
    ),
    width = width,
    height = height
  )
}
