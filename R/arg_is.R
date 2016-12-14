#' Simple interface to argument checking
#'
#' Use \code{arg_is()} as an argument to \code{check_argument()}
#'
#' @param value -- a simple value that can be tested by equality. If \code{test} is also specified,
#' the value will be used only in a hint, in which case it should be set to something meaningful
#' as a hint.
#' @param test -- a function that carries out the test,
#' e.g. match_number(), check_class(), check_data_frame()
#' @param hint if TRUE, give a more diagnostic version of the source of any error
#' @return a character string message. Length zero means the test was passed.
#'
#' @examples
#' \dontrun{
#'  test <- check_argument("grab_this + 1", arg_is(3))
#' }
#'
#' @export
arg_is <- function(value = NULL, hint = FALSE, test = NULL) {
  if (missing(value)) hint <- FALSE # can't provide a hint without a matching value
  function(X) { # the value being passed along for checking
    message <-
      if (hint) {
        sprintf("should have value %s.", as.character(value))
      } else {
        "has wrong value."
      }
    if (is.null(test) ) {
      if (is.null(value)) stop("One of either <value> or <test> args must be set.")
      if (isTRUE(all.equal(X, value, check.attributes = FALSE))) return("")
      else return(message)
    } else {
      return(test(X))
    }
    stop("shouldn't have gotten here")
  }
}
