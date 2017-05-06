#' Create a new learnr/checkr exercise or question
#'
#' Use these functions to create a blank template for a learnr exercise or question.
#' Questions are multiple choice, exercises involve code. The template will be opened in the
#' source editor on an actual file. So best to use these functions when the working directory is the place
#' where you want to store the source file.
#'
#'
#'
#' @param collection an optional character string to preface the problem id
#' @param unique_id an optional character string to use for the problem id

#' @export
new_exercise <- function(collection = NULL, unique_id = NULL) {
  new_problem(collection = collection, unique_id = unique_id,
              source_file = system.file("exercise_template.Rmd", package = "checkr"))
}
#' @rdname new_exercise
#' @export
new_question <- function(collection = NULL, unique_id = NULL) {
  new_problem(collection = collection, unique_id = unique_id,
              source_file = system.file("question_template.Rmd", package = "checkr"))
}

new_problem <- function(collection = NULL, unique_id = NULL,
                        source_file) {
  if (is.null(collection)) collection = Sys.info()[["user"]]
  if (is.null(unique_id)) unique_id <- paste(Sys.Date(), random_hex(), sep = "-")
  ID <- paste0(collection, "-", unique_id)
  template_text <- readLines(source_file)
  template_text <- gsub("\\.\\.problem_id\\.\\.", ID, template_text)
  out_name <- paste0(ID, ".Rmd")
  if (out_name %in% dir()) stop(sprintf("File with ID '%s' already exists.", out_name))
  con <- file(out_name, "w")
  writeLines(text = template_text, con = con)
  close(con)
  file.edit(out_name)
}



random_hex <- function(n=5) {
  paste(sample(c(0:9, letters[1:6]), size = n), collapse = "")
}
