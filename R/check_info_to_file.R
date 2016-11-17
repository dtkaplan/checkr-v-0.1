#' Save the checking information to a file
#'
#' Saves to a file the checking information tutor generates when a code-exercise
#' is submitted. Once saved, the information can be used for building and debugging
#' check code in the usual development environment.
#'
#' @param label info handed over by tutor
#' @param user_code same
#' @param check_code same
#' @param envir_result same
#' @param evaluate_result same
#' @param other possible arguments
#' @export
check_info_to_file <- function(label=NULL,
                            user_code = NULL,
                            check_code = NULL,
                            envir_result = NULL,
                            evaluate_result = NULL, ...) {
  save_file_name <- sprintf("~/Downloads/CheckR/chunk-%s.rds", label)
  saveRDS(list(label = label, user_code = user_code, check_code = check_code, envir = envir_result, evaluate_result = evaluate_result),
          file = save_file_name)
  tutor::feedback(paste("Check info saved in ", save_file_name), type = "success", location = "append")
}

#' @export
check_info_from_file <-
  function(chunk_name,
           directory = "~/Downloads/CheckR/") {
    fname <- sprintf("~/Downloads/CheckR/chunk-%s.rds", chunk_name)

    readRDS(fname)
}
