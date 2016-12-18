#' Qualifiers on command lines to be searched by test statements together
#'
#' @name location_qualifiers
#' @details Test sequences are put together using the \code{magrittr} chain, as in
#' \code{USER_CODE \%>\% test_1 \%>\% test_2}. The functions documented here enable you to
#' restrict the command lines being searched for a match.
#'
#' Consider these test sequences
#' \itemize{
#' \item \code{USER_CODE \%>\% test_1 \%>\% then \%>\% test_2}
#' \item \code{USER_CODE \%>\% test_1 \%>\% previously \%>\% test_2}
#' \item \code{USER_CODE \%>\% test_1 \%>\% inside \%>\% test_2}
#' }
#'
#' The \code{then} qualifier means that \code{test_2} will look at only the lines in the code
#' after the line identified by \code{test_1}. (You can use \code{then(inclusive = TRUE)} to
#' include the \code{test_1} line as well.)
#'
#' The \code{previously} qualifier means that \code{test_2} will examine only the lines that
#' preceed the line located by \code{test_1}. (Again, there is an optional \code{inclusive} argument.)
#'
#' The \code{inside} qualifier directs \code{test_2} to examine only the single line identified
#' by \code{test_1}.
#'
#' @param inclusive if \code{TRUE} then include the previously matched line in the set of
#' lines to be tested in subsequent tests
#' @param capture A set of expressions of the sort returned by capture.code(). A "capture" is the
#' kind of object taken as input to a locator test and generated as output from the test and,
#' in the end, used to set the information passed back to `tutor`.
#'
#'

#' @export
then <- function(capture, inclusive = FALSE) {
  if (length(capture$valid_lines) == 1) {
    # we're following an "inside" command
    capture$valid_lines <- capture$line:length(capture$expressions)
  }
  still_valid <-
    if (inclusive) capture$valid_lines >= capture$line
    else capture$valid_lines > capture$line
  capture$valid_lines <- capture$valid_lines[still_valid]
  capture$line <- capture$valid_lines[1]

  capture
}

#' \code{final_} finds the last command line in the code
#' No arguments need be given. Last is last!
#' @rdname location_qualifiers
#' @export
final_ <- function(capture) {
  capture$created_by <- "last result from code"
  capture$line <- max(capture$valid_lines)

  capture
}



#' @rdname location_qualifiers
#' @export
previously <- function(capture, inclusive = FALSE) {
  still_valid <- capture$valid_lines < capture$line
  capture$valid_lines <- capture$valid_lines[still_valid]
  capture$line <- capture$valid_lines[1]

  capture
}

#' @rdname location_qualifiers
#' @export
inside <- function(capture) {
  capture$valid_lines <- capture$line

  capture
}
