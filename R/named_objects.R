#' Look within the objects created and assigned by the code
#'
#' @param what A regex of the object name
#' @param description a for-the-user description of the object
#' @param from_code the code object created by capture()
#' @param go_in_reverse whether to look forward or backward
#' @param caps What to do about capitalization. Default: nothing. You can also set it to be
#' \code{tolower()} or \code{toupper()} as appropriate to match your regex
#' @export
named_object <- function(what, description, from_code, go_in_reverse = FALSE,
                         caps = I) {
  for (E in from_code$envlist) {
    if (any(grepl(what, caps(ls(E))))) return(TRUE)
  }
  res <- FALSE
  attr(res, "message") <- description

  res
}

# Change the caps to be "strict" which will be one of several functions:
# as.is
# case_indifferent
# ignore_dots_underscores


#' @export
object_value <- function(what, from_code, caps = I) {
  for (E in from_code$envlist) {
    object_names <- ls(E)
    if (length(object_names) == 0) next
    matches <- grep(what, caps(object_names))[1]
    if (any(matches)) return(get(object_names[matches], envir = E))
  }

  NULL
}
