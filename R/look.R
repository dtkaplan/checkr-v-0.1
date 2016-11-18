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
    return(list(found = FALSE, line = NA, code = code,
                message = message))
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

# call_text -- string containing what the call should
# look like, e.g. "2 + 2". Use NULL for any arguments for which
# a place is needed but for which you don't care what the value is.
#
#' @export
find_function <- function(call_text, code, message = "get a real message") {
  expanded <- as.list(parse(text = call_text)[[1]])
  the_fun <- expanded[[1]]

  result <- list()
  valid_lines <- numeric(0)
  for (k in code$valid_lines) {
    all_calls <- get_functions_in_line(code, line = k)
    inds = which(all_calls$fun_names == the_fun)
    for (j in inds) {
      result <- match_the_arguments(all_calls$args[[j]], expanded)
      if(result)
        return(list(found = TRUE, line = k, code = code, message = "" ))
    }
  }

  return(list(found = FALSE, line = NA, code = code, message = message))
}

#' @export
match_the_arguments <- function(actual, desired) {
  # does the function itself match (it should if we got this far)
  if( actual[[1]] != desired[[1]]) return(FALSE)
  # keep track of which arguments in actual we've matched with those in desired
  already_matched <- rep(FALSE, length(actual))
  # walk through the names in desired, looking for a match in actual
  for (nm in names(desired)) {
    if (nm == "") next
    match <- which(names(actual) == nm)
    if (length(match) > 0) {
      already_matched[match] <- TRUE
      if ( ! is.null(desired[[nm]])) {
        # if values don't match, we're done
        if (desired[[nm]] != actual[[match]]) {
          return(FALSE)
        } else {
        desired[[nm]] <- NULL # remove from the list
        }
      }
    }
  }
  # grab the remaining values and see if they have a match in actual
  for (k in 1:length(desired)) {
    found_it <- FALSE
    for (j in 1:length(actual)) {

      if (already_matched[j]) next

      if ( (! is.null(desired[[k]])) && (desired[[k]] == actual[[j]])) {
        found_it <- TRUE
        already_matched[j] <- TRUE
      }
    }
    if (is.null(desired[[k]])) found_it <- TRUE  # doesn't matter what the value is
    if ( ! found_it) return(FALSE)
  }

  TRUE
}

# Get a list of the functions and the arguments on
# a specified line of the expressions

#' @export
get_functions_in_line <- function(code, line) {
  EX <- code$expressions[[line]]
  res <- list(fun_names = setdiff(all.names(EX), all.vars(EX)))
  res$args <- walk_tree(EX, res$fun_names)

  res
}


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
