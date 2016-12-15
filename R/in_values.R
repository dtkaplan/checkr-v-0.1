#' Locate a match in the values created by the code
#'
#' Look for a match in the command-line values (whether or not they have a name assigned)
#' to the value or test specified.
#'
#' @details For very simple comparisons, e.g. to \code{1:10} or \code{"fred"},
#' just give the value as the \code{x} argument. More complicated comparisons, e.g. there
#' is a variable named "age" in a data frame, can be accomplished by setting \code{x} to be
#' a comparison function.
#'
#' @param x the target that you are looking for in the values.
#' Specify the match in terms of a comparison function
#' @param message a failure message
#' @param hint if \code{TRUE} give a more diagnostic failure message
#'
