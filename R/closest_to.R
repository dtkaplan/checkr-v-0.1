#' Locators based on solution code
#'
#' \code{closest_to()} finds a matching line in the solution code. It then uses the
#' value of that line (in the solution code) to look for the best-matching
#' value in the student code. The function is not to be used stand-alone, but
#' rather in the \code{\link{soln_test}} function, which calls it once for the solution
#' code and again for the user code. For the solution code, it returns the matching line
#' but for the user code it returns the line whose value matches the most closely the
#' value of the line in the solution code. So long as there is some
# value in the student code, no matter how different from the solution code's value,
# a value will be returned.
#' @param what either an object name or an identifying character string from the code generating
#' the object
#' @return a function that takes a capture as input and returns a capture as output
#' @export
closest_to <- function(what) {
  what <- substitute(what)
  if (is.character(what)) {
    # Look for a literal string in the captured code. <what> must be parsable so that
    # it can be matched to the captured code, which has been parsed.
    tmp <- try(as.character(parse(text = what)), silent = TRUE)
    if (inherits(tmp, "try-error"))
      stop(sprintf("Argument to closest_to(), '%s', is not a complete parsable statement.", what))
  }
  solution_value = NULL # used when student code is evaluated
  f <- function(capture) {

    if (is.null(solution_value)) {
      # perform the match and extract the value
      capture <-
        if(is.name(what)) {
          in_names(as.character(what))(capture)
        } else {
          in_statements(as.character(what))(capture)
        }
      solution_value <<- capture$returns[[capture$line]] # store the result
    } else {
      # find the closest matching in the capture presented
      capture$line <- find_the_closest_one(capture, solution_value)
    }
    capture
  }
  f
}

# find the closest returns in a capture to the given value
find_the_closest_one <- function(capture, val) {
  if ((!"returns" %in% names(capture)) || (length(capture$returns) == 0))
    return(NULL)
  best_score <- -Inf
  best_i <- 1
  for (k in seq_along(capture$returns)) {
    score <- 0
    X <- capture$returns[[k]]
    # class
    if (inherits(X, class(val))) score <- score + 5
    if (is.list(X) == is.list(val) ||
        is.vector(X) == is.vector(val) ||
        is.matrix(X) == is.matrix(val)) {
      score <- score + 2
      if (class(X[[1]]) == class(val[[1]])) score <- score + 2
      if (isTRUE(all.equal(X[[1]], val[[1]]))) score <- score + 3
    }
    # named elements
    obj_names <- names(val)
    if (length(obj_names) > 0) {
      score <- score + sum(names(X) %in% obj_names)
    }
    # same length result
    obj_length <- length(val)
    diff_lengths <- obj_length - length(X)
    if (diff_lengths == 0) score <- score + 3

    if (score > best_score) {
      best_score <- score
      best_i <- k
    }
  }

  best_i
}
