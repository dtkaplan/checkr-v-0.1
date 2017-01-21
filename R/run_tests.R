#' Interface to tutor
#'
#' The function that the \code{tutor} document will call when checking
#' a submission. You can tell \code{tutor} to do this with a statement in the \code{tutor}
#' document like \code{tutor_options(exercise.checker = checkr::checkr_tutor)}.
#'
#' @param label character string containing name of the chunk that's being checked. For \code{run_tests_from_file()}, this
#' will be used to refer to the saved file.
#' @param user_code character string with the student's code
#' @param solution_code character string with the code provided by the question's author.
#' @param check_code the character string contents of the chunk that specifies the
#' checks to run on the chunk being checked
#' @param envir_result the R environment after the student's code has been run
#' @param evaluate_result the output of evaluate::evaluate from the code input. Tutor provides
#' this, but checkr doesn't use it. IT MAY BE DESIRABLE TO USE IT IN THE FUTURE.
#' @param ... other arguments (see tutor documentation)
#' @param debug if \code{TRUE}, write the information in the other arguments to a file
#' \code{"~/Downloads/CheckR/chunk-[chunk-name].rds"}.
#' Then, the stored information can be put through
#' the tests outside of the tutor system, allowing better debugging. See \code{run_tests_from_file()} which
#' reads the RDS file and runs the tests.
#'
#' @return a list in the format required by tutor
#' @details The arguments are set by the tutor system. Only \code{debug} isn't
#' standard.
#'
#' @rdname tutor_interface
#' @export
checkr_tutor <- function(label=NULL,
                             user_code = NULL,
                             solution_code = NULL,
                             check_code = NULL,
                             envir_result = NULL,
                             evaluate_result = NULL, ...,
                             debug = FALSE) {
  # while debugging
  if(debug) {
    save_file_name <- sprintf("~/Downloads/CheckR/chunk-%s.rds", label)
    saveRDS(list(label = label,
                 user_code = user_code,
                 solution_code = solution_code,
                 check_code = check_code,
                 envir = envir_result,
                 evaluate_result = evaluate_result),
          file = save_file_name)
  }
  # Pre-evaluation checking
  if (is.null(envir_result)) {
    res <- pre_check_code(user_code, check_code)
    if (nchar(res) > 0)
      return(list(message = res, correct = FALSE))
    else
      return(TRUE)
  }


  # the tests
  USER_CODE <- capture.code(user_code)
  SOLN_CODE <-
    if (is.null(solution_code)) ""
    else try(capture.code(solution_code), silent = TRUE)
  if (inherits(SOLN_CODE, "try-error"))
    stop("Problem author, there is an error in the solution code.")
  test_envir <- new.env()
  assign("USER_CODE", USER_CODE, envir = test_envir)
  assign("SOLN_CODE", SOLN_CODE, envir = test_envir)
  commands <- parse(text = paste(check_code, collapse = "\n"))
  # run each of the check statements in turn
  # if the result is a capture object, see if passed is true. If not
  # signal the error right then.
  final_result <- list(correct = TRUE, message = "passed",
                       user_code = user_code,
                       type = "success", location = "prepend")
  for (k in 1:length(commands)){
    R <- eval(commands[k], envir = test_envir)
    if ( is.test_result(R) || is.capture(R)) {
      if (R$passed) next
      else {
        # failed this test
        final_result$message <- paste("Sorry, but", R$message)
        final_result$correct = FALSE
        final_result$type = "info"
        break
      }
      stop("Shouldn't get here. Statement returned neither a test result or a capture.")
    }
  }



  if (final_result$correct) {
    final_result$message <- get_success_message()
  }

  # Logging of submissions
  # maybe put a check on whether user name is set. if not, signal this to the user.
  log_entry <-
      list(user = get_user_name(), date = as.POSIXct(Sys.Date()), label = label, message = final_result$message,
            correct = final_result$correct, user_code = final_result$user_code)

  logger_fun <- options("checkr.logger")[[1]] # default, do nothing
  # but might have been set with `turn_on_google_logging(sheet_name)`
  logger_fun(log_entry) # run whatever was set

  final_result
}

#' @rdname tutor_interface
#' @export
pre_check_code <- function(user_code, check_code) {
  check_commands <- parse(text = paste(check_code, collapse = "\n"))
  test_envir <- new.env()
  assign("USER_CODE", user_code, envir = test_envir)
  for (k in seq_along(check_commands)) {
    message <- eval(check_commands[k], envir = test_envir)
    if (is.character(message) && nchar(message)) return(message)
  }
  return("")
}

