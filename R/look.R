#' Look for the line, if any, containing the sought for object. Just the
#' first such line is reported.
#'
#' @param where character string specifying whether to look at names of objects
#' assigned names, objects
#' produced by the commands (and potentially not named),
#' the statements themselves. NOT SURE WHAT ROLE commands will play.  Those are the
#' expressions as expressions.
#' @param what character string, regex, class name, number range, or name assigned
#' that we're looking for
#' @param mode what kind of thing to search for.
#'
#' @return A list with 3 elements: \code{found} indicating TRUE/FALSE whether the
#' item was found; \code{line} the line number where the item was found; \code{code}
#' the same as the input.
#' @export
find_content <- function(where = c("returns", "names", "statements", "commands"),
                         what, mode = c("class", "number", "match", "regex"),
                         message = "Test writer: give a meaningful fail message") {
  # search among the results for a match
  where <- match.arg(where)
  mode <- match.arg(mode)
  pre_process <- ifelse(where == "names",
                        ls,
                        ifelse(where %in% c("match", "regex"),
                               capture.output,
                               I)
  )
  if (mode == "number" && ! (length(what) == 2 || is.numeric(what)) )
    stop("Specify a range of 2 numbers.")
  f <- function(code) {
    if ("code" %in% names(code)) {
      # just give the code element produced by the previous step
      code <- code$code
    }
    results <- code[[where]]
    for (k in code$valid_lines) {
      content <- pre_process(results[[k]])
      if (mode == "class") {
        if (inherits(content, what))
          return(list(found = TRUE, line = k, code = code, message = ""))

      } else if (mode %in% c("match", "regex") ) {
        if (any(grepl(what, content, fixed = mode == "match")))
          return(list(found = TRUE, line = k, code = code, message = ""))
      } else if (mode == "number") {
        if (is.numeric(content) &&
            content >= min(what) &&
            content <= max(what))
          return(list(found = TRUE, line = k, code, message = ""))
      } else if (mode == "names") {
        if (any(grepl(what, content, fixed = mode == "match")))
          return(list(found = TRUE, line = k, code = code, message = "" ))
      }
    }
    return(list(found = FALSE, line = NA, code = code, message = message))
  }

  f # return the function created
}
#' @export
in_returns <- function(what, mode, message) {
  mode <- as.character(substitute(mode))
  find_content(where = "returns",
               what = what, mode = mode, message = message)
}
#' @export
in_names <- function(what, mode, message) {
  mode <- as.character(substitute(mode))
  find_content(where = "names",
               what = what, mode = mode, message = message)
}
#' @export
in_statements <- function(what, mode, message){
  mode <- as.character(substitute(mode))
  find_content(where = "statements",
               what = what, mode = mode, message = message)
}
#' @export
get_functions <- function(what, code) {
  EX <- code$expressions
  results <- list(length(EX)) # one item for each command line
  for (k in 1:length(EX)) {
    tmp <- list(fun_names = setdiff(all.names(EX[[k]]), all.vars(EX[[k]])))
    tmp$args <- walk_tree(EX[[k]], tmp$fun_names)
    results[[k]] <- tmp
  }
  results
}

#' @export
walk_tree <- function(EX, fun_names) {
  # get the function and arguments at the highest level
  if (length(EX) == 1) {
    if (any(EX == fun_names)) res <- as.character(EX)
    else res <- NULL
  } else if (any(EX[[1]] == fun_names)) {
    res <- list(as.list(EX))
    if (length(EX) > 1) res <- c(res, walk_tree(EX[[2]], fun_names))
    if (length(EX) == 3) res <- c(res, walk_tree(EX[[3]], fun_names))
  } else {
    res <- NULL
  }

  res
}
