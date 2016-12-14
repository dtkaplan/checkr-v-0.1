# Defunct function from look.R



# @param where character string specifying whether to look at names of objects
# assigned names, objects
# produced by the commands (and potentially not named),
# the statements themselves. NOT SURE WHAT ROLE commands will play.  Those are the
# expressions as expressions.
# @param what character string, regex, class name, number range, or name assigned
# that we're looking for
# @param mode what kind of thing to search for.
# @param mistake to indicate that, to pass,
# the pattern should not be found. Can be used to check for common
# mistakes.
#
# @return A list with 3 elements: \code{passed} indicating TRUE/FALSE whether the
# test was passed; \code{line} the line number where the item was found; \code{code}
# the same as the input.
# @export
find_content <- function(where = c("returns", "names", "statements", "commands"),
                         what, mode = c("class", "number", "match", "regex"),
                         message = "Test writer: give a meaningful fail message",
                         mistake = FALSE, ...) {
  # search among the results for a match
  where <- match.arg(where)
  mode <- match.arg(mode)
  # If looking for an exact match, parse the "what" in order
  # to be able to compare it to the captured code, which will have been parsed
  if (mode == "match") {
    tmp <- try(as.character(parse(text = what)), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      tmp <- what
      stop(sprintf("String '%s' for matching is not parsable", what))
    } else {
      what <- tmp
    }

  }
  pre_process <- ifelse(where == "names",
                        ls,
                        ifelse(where %in% c("match", "regex"),
                               capture.output,
                               I)
  )
  if (mode == "number" && ! (length(what) == 2 || is.numeric(what)) )
    stop("Specify a range of 2 numbers.")
  success <- !mistake
  success_message <- ifelse(mistake, message, "")
  fail_message <- ifelse(mistake, "", message)
  f <- function(capture) {
    capture$created_by <-
      if (where == "names") sprintf("that creates an object named '%s'.", what)
    else if (where == "returns") sprintf("that returns an object like '%s'.", what)
    else if (where %in% c("statements", "commands")) sprintf("that contains a command like '%s'.", what)
    else "this is a meaningless 'created_by' message. Shouldn't be happening."

    if ( ! capture$pass) return(capture) # short circuit if non-passing input
    success_flag <- FALSE
    found_in_line <- NA
    search_in <- capture[[where]]
    for (k in capture$valid_lines) {
      content <- pre_process(search_in[[k]])
      if (mode == "class") {
        if (inherits(content, what)) {
          success_flag <- TRUE; found_in_line <- k; break
        }
      } else if (mode %in% c("match", "regex") ) {
        if (where == "names" && (! what %in% content)) {
          # This is crazy verbose. This function should be simplifed, or broken into
          # more task-specific functions.
          success_flag <- FALSE; found_in_line <- k; break
        } else if (grepl(what, content, fixed = mode == "match")){
          success_flag <- TRUE; found_in_line <- k; break
        }
      } else if (mode == "number") {
        if (is.numeric(content) &&
            content >= min(what) &&
            content <= max(what)) {
          success_flag <- TRUE; found_in_line <- k; break
        }
      }
    }
    capture$line <- found_in_line
    if (success_flag) {
      capture$passed = success
      capture$message = success_message
    } else {
      capture$passed = ! success
      capture$message = fail_message
    }

    capture
  }

  f # return the function created
}

#'

#'
# These are factories for test functions. Test functions take either a result or the
# output of capture.output() as inputs. Test functions always return a result.
# a result has fields <passed>, <message>, <code>, <lines>. The output of capture.code
#' @rdname look

in_factory <- function(where) {
  f <- function(what, message = NULL, regex = NULL, number = NULL, class = NULL, mistake = FALSE, ...) {
    mode = "match"
    if (!is.null(regex)) {
      mode <- "regex"
      what <- regex
    }
    if (!is.null(number)) {
      mode <- "number"
      what <- number
    }
    if (!is.null(class)) {
      mode <- "class"
      what <- class
    }
    if (is.null(message)) {
      message <-
        sprintf("couldn't find match to %s'%s'",
                ifelse(mode == "number", "number ",
                       ifelse(mode == "class", "class ",
                              ifelse(mode == "regex", "regex ", ""))),
                as.character(what))
    }

    find_content(where = where, mode = mode, what = what,
                 message = message, regex = regex, number = number, class = class, ...)
  }
  f
}
