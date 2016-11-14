#' evaluate R statements, capturing code, created vars, and printed output
#'
#' Each expression in the code is evaluated in it's own environment, with the
#' previous expression's environment as the parent. That way, you can examine
#' what's changed after each statement.
#'
#' searching for objects in the last environment will move as needed to the
#' parent environments, and so will give the most recent value of each object.
#'
#' @param dot_R_file the name of the file containing the code as text
#' @param example the name of the example to run (this is just for testing and should
#' be replaced by a \code{text = } argument for production), which is an alternative
#' to \code{dot_R_file}.
#' @return a list containing 1) the \code{results} of each command (which might not have been
#' saved in an object in the statement itself), 2) a list \code{envlist}
#' holding the environments created for each command, 3) the statements themselves as
#' character strings, one string for each command regardless of how many lines
#' it was spread over in the code, and 4) the statements as expressions (which might
#' be evaluated in checking code, or parsed, or whatever).
#' @examples
#' foo <- capture.code(example = "example_0.R")
#' @export
capture.code <- function(text = NULL, dot_R_file = NULL, example = NULL) {
  # Get the code from its file or text form
  if ( ! is.null(text)) {
    code <- text
  } else {
    if (is.null(dot_R_file) && ! is.null(example))
      dot_R_file <- system.file(example, package = "checkr")
    code <- readLines(dot_R_file)
  }
  # turn it into evaluatable commands
  commands <- parse(text = paste(code, collapse = "\n"))

  R <- list()
  # P <- character(0) #not used. Print the R items instead.

  environments <- list()
  # Get the source code
  statements <- attr(commands, "srcref")
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

  list(results = R,
       envlist = environments,
       statements = statements,
       commands = commands)
}
