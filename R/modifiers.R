#' modifiers on pass objects
#'
#' These functions let the test-writer specify whether it's sufficient that a test be passed
#' and whether it's required (\code{needs()}) that a test pass.

#' @return the modified pass object
#' @param pass a pass or capture object
#' @rdname modifiers
#' @export
needs <- function(pass) {
  if (!pass$pass) pass$stop <- TRUE
  pass
}
#' @rdname modifiers
#' @export
suffices <- function(pass) {
  pass$suffices <- TRUE
  pass
}

