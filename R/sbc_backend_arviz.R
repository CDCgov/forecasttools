#' SBC Backend for Arviz
#'
#' This function creates an SBC backend for Arviz. This backend is used to hold
#' the arguments that will be passed to the Arviz backend, mainly the path to
#' the Arviz datafile. The other purpose is to extend the S3 generics from the
#' SBC package.
#'
#' @param ... Arguments to be passed to the backend. The argument 'data' should
#' not be provided as it needs to be set by the SBC package. The argument 'path'
#' (to the Arviz datafile) must be provided.
#'
#' @return A list with the provided arguments, structured as an SBC backend for
#' Arviz.
#'
#' @examples
#' \dontrun{
#' sbc_backend_arviz(path = "path/to/arviz_datafile")
#' }
#'
#' @export
sbc_backend_arviz <- function(...) {
  args <- list(...)
  if (any(names(args) == "data")) {
    stop("Argument 'data' cannot be provided when defining a backend as it needs
    to be set by the SBC package")
  }
  if (!any(names(args) == "path")) {
    stop(paste0("Argument 'path' (to the arviz datafile) must be provided when
    defining a backend"))
  }

  structure(list(args = args), class = "sbc_backend_arviz")
}
