#' Run the tests from info produced by tutor
#'
#' @param tutor_obj the checking object produced by tutor
#' You can also use the result of check_info_from_file()
#' @export
run_tests <- function(label=NULL,
                             user_code = NULL,
                             check_code = NULL,
                             envir_result = NULL,
                             evaluate_result = NULL, ...,
                             feedback = tutor::feedback,
                             debug = TRUE) {
  # while debugging
  if(debug) {
    save_file_name <- sprintf("~/Downloads/CheckR/chunk-%s.rds", label)
    saveRDS(list(label = label, user_code = user_code, check_code = check_code, envir = envir_result, evaluate_result = evaluate_result),
          file = save_file_name)
  }
  # the tests
  USER_CODE <- capture.code(user_code)
  test_envir <- new.env()
  assign("USER_CODE", USER_CODE, envir = test_envir)
  commands <- parse(text = paste(check_code, collapse = "\n"))
  # run each of the check statements in turn
  # if the result is a capture object, see if passed is true. If not
  # signal the error right then.
  for (k in 1:length(commands)){
    R <- eval(commands[k], envir = test_envir)
    if ( ! is.null(names(R)) && all(c("passed", "line", "message") %in% names(R))) {
      if (R$passed) next
      else {
        # failed this test
        return(feedback(paste("Sorry, but", R$message), type = "info", location = "prepend"))

      }

    }
  }

  feedback("Good job!", type = "success")
}

#' @export
run_tests_from_file <- function(label) {
  raw <- check_info_from_file(label)
  with(raw, run_tests(label = label, user_code = user_code,
                             check_code = check_code,
                             envir_result = envir_result,
                             evaluate_result = evaluate_result,
                             feedback = function(m, ...) m, # just return the feedback message
                             debug = FALSE))
}
