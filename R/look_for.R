#' Look for some given content in the printed form of the objects created
#'
#' Redo this so that look_for() et al. return objects which can be operated on by
#' and, or, etc.
#'
#'
#' @param what a character string. The string is treated as a regex.
#' @param description a character string describing to the student what is being sought
#' @param results the object produced by capture()
#' @param go_in_reverse (logical) the order in which to assess the results
#' @export
look_for <- function(what = NULL,
                     description = " ",
                     from_code, go_in_reverse = FALSE, where = "results") {
  if (length(what) > 1 || !is.character(what))
    stop("The what argument must be a single string. Rewrite your test.")
  # Fix <what> to allow for any number of blanks
  what <- gsub(" +", "\\\\s+", what)
  inds <- 1:length(from_code$results)
  if (go_in_reverse) inds <- rev(inds)
  for (k in inds) {
    output <-
      if (where == "results" ) capture.output(print(from_code[["results"]][[k]]))
      else if (where == "statements") from_code$statements[k]
    if (any(grepl(what, output))) return(TRUE)
  }
  res <- FALSE
  attr(res, "message") <- description

  res
}


# Look for a quoted string
#' @export
quoted_string <- function(str) {
  paste0("[\"\']", str, "[\"\']")
}

#'@ export
#' @export
in_input <- function(what, description = "", from_code, go_in_reverse = FALSE) {
  look_for(what = what, description = description,
           from_code = from_code, go_in_reverse = go_in_reverse,
           where = "statements")

}
#' @export
in_output <- function(what, description = "", from_code, go_in_reverse = FALSE) {
  look_for(what = what, description = description,
           from_code = from_code, go_in_reverse = go_in_reverse,
           where = "results")

}


