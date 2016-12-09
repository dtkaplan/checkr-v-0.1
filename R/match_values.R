#' supervisory function for matching output values
#'
#' @param capture the evaluated capture object from the submitted code
#' @param solution the evaluated capture object from the solution code
#' @param ... lists of statements giving names to the objects to be matched
#' and the requirements framed on the objects created in \code{assigns}
#' @export
match_values <- function(student = NULL, solution = NULL, ...) {
  statements <- lazyeval::lazy_dots(...)
  R <- new_result()
  if (is.null(names(statements))) stop("Must name some objects for comparison")

  submitted_vals <- list() # The values of the objects created in the submitted code
  official_vals <- list() # The values of the objects created in the solution code
  where_from <- list() # The tests to look for the object
  for (k in 1:length(statements)) {
    # calculate the statement in the context of those already calculated
    nm <- names(statements)[k]
    if (nm != "") { # We're creating a named object. Calculate it's value
      # find corresponding value in <submitted_vals> by evaluating
      # the locator test
      to_do <- eval(statements[[nm]]$expr, envir = submitted_vals)
      tmp <-
        if (is.function(to_do)) do.call(to_do, list(student))
      else to_do

      if (is.capture(tmp)) {
        if (!tmp$passed) {
          # Couldn't find the item specified by the locator test
          R$message <- paste0(R$message, "Couldn't find a command ", tmp$created_by)
          R$passed <- FALSE
          return(R) # we're done if we can't find an item
        }
        tmp <- get_line_value(tmp)
      }
      submitted_vals[[nm]] <- tmp

      # find corresponding value in <official_vals>
      to_do <- eval(statements[[nm]]$expr, envir = official_vals)
      tmp <-
        if (is.function(to_do)) do.call(to_do, list(solution))
      else to_do
      if (is.capture(tmp)) {
        # throw an error if the solution does not pass the locator test
        if(!tmp$passed) stop("Nothing in solution ", tmp$created_by)
        tmp <- get_line_value(tmp)
      }

      official_vals[[nm]] <- tmp
    } else { # run the comparison tests specified

      # What will the comparison tests look like?
      # numeric(b, range, tol=1e-8)
      #  will get the value of b from both submitted and official, and run some comparison
      #  function on them with the specified arguments.
      test <- statements[[k]]
      this_test <- eval(test$expr)
      thisR <- do.call(this_test, list(submitted_vals, official_vals))
      # R should be a "test_result" object
      if (! inherits(thisR, "test_result"))
        stop("Internal error: should have created a test_result object")
      if (thisR$stop && !thisR$passed) {
        return(thisR)
      }
      if (thisR$suffice && thisR$passed) return(thisR) # immediate success
      if (!thisR$passed) {R$message <-
        paste0(R$message, ". ",
               paste0("'", all.vars(test$expr), "'",collapse = ', '),
               " ", thisR$message, ".")
      }
    }
  }

  return(thisR)
}





