# My original idea was to walk down the chain, applying tests to
# it one step at a time. But now I've revised that to break up the chain
# into single steps. That way, the normal test functions can be used.
# So I don't need these files anymore.

#' tests on magrittr chains
# Look for a sequence of function calls in a chain
# ... is alternating tests and messages
# Set up to work just for the first valid line

#' @export
chain_test <- function(...) {
  # arguments are tests and messages
  tests_and_messages <- unlist(list(...))
  f <- function(capture) {

    result <-
      functions_in_chain(
        unlist(capture$statements[[capture$valid_lines[1]]]),
        tests_and_messages)
    if (result == "") {
      capture$passed = TRUE
      capture$line = capture$valid_lines[1]
      capture$message = ""
    } else {
      capture$passed = FALSE
      capture$line = NA
      capture$message = result
    }

    capture
  }

  f
}

# THIS IS WHERE THE PROBLEM IS HAPPENING WHEN RUNNING
# the checkr-test-cases problem 3

functions_in_chain <- function(chain_string, ...) {
  tests_and_messages <- unlist(list(...))

  tests <-
    tests_and_messages[seq(1, length(tests_and_messages), by = 2)]
  messages <-
    tests_and_messages[seq(2, length(tests_and_messages), by = 2)]

  step_text <- unlist(strsplit(chain_string, split = "%>%"))
  steps <-
    parse(text =
            gsub("%>%", "\n", chain_string))

  current_step <- 1
  last_passed <- 0
  for (k in 1:length(tests)) {
    matches <- FALSE
    expanded <- as.list(parse(text = tests[k])[[1]])
    for (j in current_step:length(steps)) {
      # match test against step j
      matches <-
        match_the_arguments(
          as.list(steps[[j]]), expanded)

      # if a match, move on to the next test
      if (matches) {
        current_step <- j
        last_passed <- j
        break
      }
    }
    if (!matches) {
      # construct the statement, e.g.
      # Got to step[last_passed] but ... the message
      if (last_passed > 0) {
        return(
          paste("Good up to", step_text[last_passed],
                        "but", messages[k], "after."))
      } else {
        return(paste("In chain,", messages[k]))
      }
      return(final_message)
    }
  }

  return("")
}

# This was brought back from 40dbe758
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

      if (already_matched[j] || found_it) next

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

