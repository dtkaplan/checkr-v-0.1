#' evaluate R statements, capturing code, created vars, and printed output
#'
#' Each expression in the code is evaluated in it's own environment, with the
#' previous expression's environment as the parent. That way, you can examine
#' what's changed after each statement.
#'
#' Searching for objects in the last environment will move as needed to the
#' parent environments, and so will give the most recent value of each object.
#'
#' @param code_text the commands being evaluated as pure text
#' @return a list containing 1) the \code{results} of each command (which might not have been
#' saved in an object in the statement itself), 2) a list \code{envlist}
#' holding the environments created for each command, 3) the statements themselves as
#' character strings, one string for each command regardless of how many lines
#' it was spread over in the code, and 4) the statements as expressions (which might
#' be evaluated in checking code, or parsed, or whatever). Note that all of the elements in the
#' list have the same number of components, which need to be accessed with \code{[[]]} to get
#' individual commands
#'
#' @examples
#' capture.code("x <- 2 + 2\n#then square it\ny <- x^2")
#' capture.code("for (k in 1:2) {\n x <- k\nsqrt(7)}")
#' @export
capture.code <- function(code_text = NULL) {
  # Get the code from its file or text form
  # turn it into evaluatable commands
  commands <- parse(text = paste(code_text, collapse = "\n"))

  R <- list()
  # P <- character(0) #not used. Print the R items instead.

  environments <- list()
  # Get the source code
  statements <- as.character(commands) #attr(commands, "srcref")
  # Run  the commands one at a time
  for (k in 1:length(commands)) {
    if (k > 1) {
      parent_environment <- environments[[k - 1]]
    } else {
      parent_environment <- parent.frame()
    }
    environments[[k]] <- new.env(parent = parent_environment)
    # capture the unnamed objects created in this step of code
    R[[k]] <- eval(commands[k], envir = environments[[k]])
  }

  list(returns = R,
       names = environments,
       statements = lapply(statements, FUN = as.character),
       expressions = commands,
       valid_lines = 1:length(R),
       passed = TRUE,
       line = 0,
       message = "")
}
