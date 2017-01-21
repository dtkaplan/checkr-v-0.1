#' evaluate R statements, capturing code, created vars, and printed output
#'
#' @details Each expression in the code is evaluated in it's own environment, with the
#' previous expression's environment as the parent. That way, you can examine
#' what's changed after each statement.
#'
#' Searching for objects in the last environment will move as needed to the
#' parent environments, and so will give the most recent value of each object.
#'
#' @param code_text the commands being evaluated as pure text
#' @param x the object to be checked
#' @return a list containing 1) the \code{results} of each command (which might not have been
#' saved in an object in the statement itself), 2) a list \code{envlist}
#' holding the environments created for each command, 3) the statements themselves as
#' character strings, one string for each command regardless of how many lines
#' it was spread over in the code, and 4) the statements as expressions (which might
#' be evaluated in checking code, or parsed, or whatever). Note that all of the elements in the
#' list have the same number of components, which need to be accessed with \code{[[]]} to get
#' individual commands
#'
# @importFrom lazyeval lazy_dots
#' @importFrom stats terms.formula
#' @importFrom utils capture.output file.edit
# # @importFrom sourcetools tokenize_string
#' @import magrittr
#' @import googlesheets
# ' @importFrom jsonlite toJSON
#'
#' @examples
#' capture.code("x <- 2 + 2\n#then square it\ny <- x^2")
#' capture.code("for (k in 1:2) {\n x <- k\nsqrt(7)}")
#' @rdname capture
#' @export
capture.code <- function(code_text = NULL) {
  # Get the code from its file or text form
  # turn it into individual evaluatable commands, one per line
  code_text <- as.character(parse(text = code_text))
  # deconstruct any magrittr chains
  for (k in seq_along(code_text)) {
    code_text[k] <- expand_chain(code_text[k])
  }
  code_text <- paste0(code_text, "\n")

  commands <- parse(text = paste(code_text, collapse = "\n"))

  R <- list()
  # P <- character(0) #not used. Print the R items instead.

  environments <- list()
  # Get the source code
  statements <- as.character(commands) #attr(commands, "srcref")
  # Run  the commands one at a time
  for (k in seq_along(commands)) {
    if (k > 1) {
      parent_environment <- environments[[k - 1]]
    } else {
      parent_environment <- parent.frame()
    }
    environments[[k]] <- new.env(parent = parent_environment)
    # capture the unnamed objects created in this step of code
    res <- try(
      eval(commands[k], envir = environments[[k]]),
      silent = TRUE)
    R[[k]] <- res
  }

  statements <- lapply(statements, FUN = as.character)
  # too simple fix to parse magrittr statements
  # put in the . argument explicitly
  # problem: Doesn't check for already existing . arguments
  # Taking it out
  #  statements <- gsub("%>% (+[a-zA-z._]*\\()", "%>% \\1\\.,", statements)

  res <-
    list(returns = R,
         names = environments,
         statements = statements,
         expressions = commands,
         valid_lines = 1:length(R),
         passed = TRUE,
         line = 0,
         message = "",
         created_by = "capturing code")
  class(res) <- "capture"

  res
}

# helper function to identify capture objects

#' @rdname capture
#' @export
is.capture <- function(x) inherits(x, "capture")

# helper function to get the value produced by a line of submitted code

get_line_value <- function(cap) {
  if (! is.capture(cap)) stop("Not a capture object")
  res <- cap$returns[[cap$line]]
  if (is.null(res)) res <- "no such command found"

  res
}
