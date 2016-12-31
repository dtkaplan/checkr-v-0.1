#' Authenticate a user
#'
#' Use this function to insert into a tutor document the user input fields for
#' user ID and password. This is done in a simple, but unusual way: by creating an empty chunk with
#' a `child` chunk attribute like this: \code{child = checkr::authentication()}.
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
