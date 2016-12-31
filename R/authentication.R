#' Authenticate a user
#'
#' Use this function to insert into a tutor document the user input fields for
#' user ID and password.
#'
#'
#' @rdname message_passing
#' @param password_file The name of a file containing user-ID/password pairs to use in the
#' authentication. This is intended for future use.
#' @param ... other arguments for future use.
#' @export
authentication <- function(password_file, ...) {
  system.file("authentication.Rmd", package = "checkr")
}
