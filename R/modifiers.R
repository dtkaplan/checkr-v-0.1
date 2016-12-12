#' modifiers on pass objects
#'
#' These functions let the test-writer specify whether it's sufficient that a test be passed
#' and whether it's required (\code{needs()}) that a test pass.

#' @return the modified pass object
#' @param capture a capture object
#' @param pass a pass object
#' @export
needs <- function(pass) {
  if (!pass$pass) pass$stop <- TRUE
  pass
}
#' @export
suffices <- function(pass) {
  pass$suffices <- TRUE
  pass
}

