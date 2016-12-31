#' message passing from tests
#'
#' for communicating back from the tests to the final result.
#' @param message a character string message to signal success to the user.
#' @param user_name a character string identifying the user.
#'
#' @details \code{set_user_name} and \code{get_user_name} are used by the logging system
#'
#' @rdname message_passing
#' @export
set_success_message <- function(message) {
  assign("success_message", message, envir = message_env)
}

#' @rdname message_passing
#' @export
get_success_message <- function() {
  message <- "Good job!"
  tmp <- ""
  if ("success_message" %in% names(message_env)) {
    tmp <- get("success_message", envir = message_env)
    assign("success_message", "", envir = message_env)
  }

  if (tmp != "") return(tmp)
  else return(message)
}

message_env <- new.env()

#' @rdname message_passing
#' @export
set_user_name <- function(user_name) {
  assign("user_name", user_name, envir = message_env)
}

#' @rdname message_passing
#' @export
get_user_name <- function() {
  if ("user_name" %in% names(message_env)) {
    return(get("user_name", envir = message_env))
  } else {
    return("")
  }
}

