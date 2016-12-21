#' message passing from tests
#'
#' for communicating back from the tests to the final result.
#' @param message a character string message to signal success to the user.
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
    cat(sprintf("recovered '%s' from message_env\n", tmp))
    assign("success_message", "", envir = message_env)
  }

  if (tmp != "") return(tmp)
  else return(message)
}

message_env <- new.env()
