#' Checking values of objects created or displayed by the code
#'
#' These functions capture the object itself. If there is no such object, a character
#' string with the message is returned. That message will have a 'no_match' attribute
#'
#' If the test passes, the value is returned in the "value" attribute of the capture object
#'
# #' @param capture the code object
# #' @param what the object name, class(es), function/argument string, or text to match
# #' @param strict whether the matching name or text can be approximate
# #' (e.g. capitalization, dots, underscores, ...)
#
# # The user interface: identify a line in a capture, then pass the resulting capture to
# # check_value()

#' @param capture a capture object
#' @param test a function taking one value as an argument. The test to run to evaluate the captured value. Should
#' return \code{""} if passing, non-empty message string if not
#' @param message the message to give if the test fails
#' @export
check_value <- function(test, message = NULL, mistake = FALSE) {
  test_text <- deparse(substitute(test))
  if (is.null(message))
    message <- ifelse(mistake,
      sprintf("test '%s' shouldn't pass", text_text),
      sprintf("test '%s' failed", test_text))
  success_message <- ! mistake # if the mistaken test is passed, the test fails
  f <- function(capture) {
    if ( ! capture$pass) return(capture) # non-passing input given, so don't do test

    if (is.na(capture$line) || (! capture$line %in% capture$valid_lines)) {
      stop("Using check_value() without a valid capture$line. Give a preliminary test to identify the line who's value is sought.")
    }
    value <- capture$returns[[capture$line]]
    result <- test(value)
    if ((mistake && result == "") || result != "") {
      # either the mistaken pattern was found, so the test should fail
      # or the pattern was not found when it should have been (mistake == TRUE) and
      # so the test fails
      capture$passed = FALSE
      capture$message = paste(message, sprintf("for value of line '%s'", capture$statements[capture$line]))
    }

    capture
  }
  f
}

# THIS NEEDS TO BE FOLDED INTO in_names()

# but doesn't need to be exported
get_match_ind <- function(what, nms, strict = TRUE) {
  if (strict) {
    return(which(nms == what))
  } else {
    # change everything to lower case
    # remove dots and underscores from names
    what <- tolower(gsub("[\\.|_]*", "", what))
    nms <- tolower(gsub("[\\.|_]*", "", nms))
    return(get_match_ind(what, nms, strict = TRUE))
  }
}


# Like check_argument(), but just grabs the value of the argument.
grab_argument <- function(arg_spec) {
  expanded <- as.list(parse(text = arg_spec)[[1]])
  message <- sprintf("couldn't find match to %s", arg_spec)
  the_fun <- expanded[[1]]
  R <- new_test_result()
  f <- function(capture) {
    if ( ! capture$passed) {
      R$passed <- FALSE
      R$message <- message
      R$has_value <- FALSE
      return(R)
    } # short circuit the test
    if ( ! capture$line %in% capture$valid_lines)
      stop("Test designer should specify a previous test that finds the line to examine.")
    all_calls <- get_functions_in_line(capture$expressions, line = capture$line)
    inds = which(all_calls$fun_names == the_fun)
    for (j in inds) {
      call_to_check <- as.call(parse(text = as.character(all_calls$args[[j]])))
      result <- corresponding_arguments(call_to_check, expanded)
      if (length(result$grabbed) ) {
        R$value <- eval(result$grabbed[[1]], envir = capture$names[[capture$line]])
        R$has_value <- TRUE
        return(R)
      }
    }

    R
  }
  f
}





get_function_from_call <- function(call) {
  res <- call[[1]]
  if (is.name(res)) res <- get(as.character(res))

  res
}



# create a correpondance between the arguments in two function calls
corresponding_arguments <- function(one, reference) {
  get_arg_list <- function(the_call) {
    # expand the call so that all arguments have their
    # corresponding names in the formal (or a number for primitives)
    if (is.expression(the_call)) the_call <- as.call(the_call[[1]])
    the_fun <- get_function_from_call(the_call)
    if (is.primitive(the_fun)) {
      result <- as.list(the_call)[-1]
      if(length(result) > 0) names(result) <- 1:length(result)
    } else {
      if (is.list(the_call)) the_call <- as.call(the_call)
      expanded <- match.call(the_fun, the_call)
      result <- as.list(expanded)[-1]
    }

    result
  }
  args_one <- get_arg_list(one)
  args_ref <- get_arg_list(reference)

  grabbed <- list()
  mismatch <- character(0)
  missing <- character(0)

  for (nm in names(args_ref)) {
    if ( ! nm %in% names(args_one)) {
      # keep track of which arguments didn't match
      missing[length(missing) + 1] <- nm
      next
    }
    if (is.call(args_ref[[nm]]) && args_ref[[nm]][[1]] == as.name("eval"))
      args_ref[[nm]] <- eval(args_ref[[nm]])
    if (all(args_ref[[nm]] == as.name("whatever"))) next # we're not concerned about the value
    if (all(args_ref[[nm]] == as.name("grab_this"))) {
      grabbed[[nm]] <- args_one[[nm]]
    } else {
      # check to see if the values match
      if (is.call(args_ref[[nm]]) && is.call(args_one[[nm]])) {
        # function call, so recurse
        result <- corresponding_arguments(args_one[[nm]], args_ref[[nm]])
        grabbed <- c(grabbed, result$grabbed)
        missing <- c(missing, result$missing)
        mismatch <- c(mismatch, result$mismatch)
      } else if (is.name(args_ref[[nm]]) && is.name(args_one[[nm]])){
        if (! identical(args_ref[[nm]], args_one[[nm]]))
          mismatch[length(mismatch) + 1] <- nm
      } else {
        if (is.call(args_ref[[nm]])) args_ref[[nm]] <- eval(args_ref[[nm]])
        if (is.call(args_one[[nm]])) args_one[[nm]] <- eval(args_one[[nm]])
        if (! isTRUE(all.equal(args_ref[[nm]], args_one[[nm]])))
          mismatch[length(mismatch) + 1] <- nm
      }
    }
  }
  return(list(grabbed = grabbed, missing = missing, mismatch = mismatch))
}

# flags for argument grabbing
#' @export
grab_this <- as.name("grab_this")
#' @export
whatever <- as.name("whatever")
