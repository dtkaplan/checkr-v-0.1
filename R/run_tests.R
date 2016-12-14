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
#' NOTE NOTE NOTE I would like to have the R environment *before* the student code is run. That way,
#' I can run the solution_code in the same environment seen by the student code.
#' @param evaluate_result the output of evaluate::evaluate from the code input. Tutor provides
#' this, but checkr doesn't use it. IT MAY BE DESIRABLE TO USE IT IN THE FUTURE.
#' @param ... other arguments (see tutor documentation)
#' @param feedback a function (provided by tutor) to convey a feedback message to the student
#' via the tutor document
#' @param debug if \code{TRUE}, write the information in the other arguments to a file
#' \code{"~/Downloads/CheckR/chunk-[chunk-name].rds"}.
#' Then, the stored information can be put through
#' the tests outside of the tutor system, allowing better debugging. See \code{run_tests_from_file()} which
#' reads the RDS file and runs the tests.
#'
#' @details The arguments are set by the tutor system. Only \code{debug} isn't
#' standard.
#'
#' @export
checkr_tutor <- function(label=NULL,
                             user_code = NULL,
                             solution_code = NULL,
                             check_code = NULL,
                             envir_result = NULL,
                             evaluate_result = NULL, ...,
                             feedback = tutor::feedback,
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
  # the tests
  USER_CODE <- capture.code(user_code)
  SOLN_CODE <- capture.code(solution_code)
  test_envir <- new.env()
  assign("USER_CODE", USER_CODE, envir = test_envir)
  assign("SOLN_CODE", SOLN_CODE, envir = test_envir)
  commands <- parse(text = paste(check_code, collapse = "\n"))
  # run each of the check statements in turn
  # if the result is a capture object, see if passed is true. If not
  # signal the error right then.
  for (k in 1:length(commands)){
    R <- eval(commands[k], envir = test_envir)
    if ( is.test_result(R) || is.capture(R)) {
      if (R$passed) next
      else {
        # failed this test
        return(feedback(paste("Sorry, but", R$message), correct = FALSE, type = "info", location = "prepend"))

      }
      stop("Statement returned neither a test result or a capture.")
    }
  }

  feedback("Good job!", correct = TRUE, type = "success")
}
