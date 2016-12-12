#' Simple interface to argument checking
#'
#' Use \code{arg_is()} as an argument to \code{check_argument()}
#'
#' @param value -- a simple value that can be tested by equality
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
arg_is <- function(value, hint = FALSE, test = NULL) {
  function(X) { # the value being passed along for checking
    fspec <- attr(X, "fspec")
    if (is.null(fspec)) fspec <- value
    else fspec <- gsub("grab_this", "_____",  fspec)
    message <-
      if (hint) {
        sprintf("to %s should have value %s.",
                fspec, as.character(value))
      } else {
        sprintf("to %s has wrong value.", fspec)
      }
    if (is.null(test) ) {
      if (isTRUE(all.equal(X, value, check.attributes = FALSE))) return("")
      else return(message)
    } else {
      return(test(X))
    }
    stop("shouldn't have gotten here")
  }
}
