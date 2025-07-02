#' Helper functions for interacting with APIs

#' Warn the user that they are attempting an API request
#' without credentials
#'
#' @param api_key_creation_url Optional URL to which to direct
#' the user to create an API key ID / secret pair.
#' @return Nothing, invisibly, warning the user as a side effect.
#'
#' @keywords internal
.warn_no_api_creds <- function(api_key_creation_url = NULL) {
  if (!is.null(api_key_creation_url)) {
    create_creds_msg <- glue::glue(
      " Create an ",
      "API key ID / secret pair by visiting ",
      "{api_key_creation_url}."
    )
  } else {
    create_creds_msg <- NULL
  }
  cli::cli_warn(c(
    glue::glue(
      "No valid API key ID / secret pair provided. ",
      "This is considered impolite and ",
      "may result in your requests to the ",
      "server getting throttled."
    ),
    create_creds_msg
  ))

  invisible()
}

#' Default API requests for forecasttools via
#' httr2
#'
#' @param url URL for the request to perform, passed as the `url`
#' argument to [httr2::request()].
#' @param api_key_id API key id to use when authenticating.
#' If `NULL` or the empty string (`""`), treated as not provided.
#' @param api_key_secret API key secret to use when authenticating.
#' If `NULL` or the empty string (`""`), treated as not provided.
#' @param api_key_creation_url The user will be warned if they fail
#' to provide an `api_key_id` and `api_key_secret`. Provide this
#' optional argument to direct the user to a website where they can
#' create those credentials. See [.warn_no_api_creds()].
#' @keywords internal
.perform_api_request <- function(
  url,
  api_key_id,
  api_key_secret,
  api_key_creation_url = NULL
) {
  to_perform <-
    httr2::request(url) |>
    httr2::req_user_agent(
      paste0(
        "forecasttools R package ",
        "(https://cdcgov.github.io/forecasttools)"
      )
    )

  api_key_id <- api_key_id %||% ""
  api_key_secret <- api_key_secret %||% ""
  has_credentials <- all(c(api_key_id, api_key_secret) != "")

  if (has_credentials) {
    to_perform <- httr2::req_auth_basic(
      to_perform,
      api_key_id,
      api_key_secret
    )
  } else {
    .warn_no_api_creds(api_key_creation_url)
  }

  response <- httr2::req_perform(to_perform)
  return(response)
}
