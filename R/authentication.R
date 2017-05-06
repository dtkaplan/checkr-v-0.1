#' Authenticate a user
#'
#' Use this function to insert into a learnr document the user input fields for
#' user ID and password. This is done in a simple, but unusual way: by creating an empty chunk with
#' a `child` chunk attribute like this: \code{child = checkr::authentication()}.
#'
#'
#' @rdname message_passing
#' @param active if \code{TRUE} turn authentication on. Otherwise, don't include authentication.
#' @param password_file The name of a file containing user-ID/password pairs to use in the
#' authentication. This is intended for future use.
#' @param ... other arguments for future use.
#' @export
authentication <- function(active = TRUE, password_file, ...) {
  if (active) system.file("authentication.Rmd", package = "checkr")
  else system.file("authentication-off.Rmd", package = "checkr")
}
