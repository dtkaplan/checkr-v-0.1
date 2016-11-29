#' checks on formula's



# check that the formula includes specified terms
# (there may be additional ones)
#' @export
f_has_terms <- function(student, term_formula) {
  res <- f_same_explanatory(student, term_formula)

  attr(res, "missing")
}

#' @export
f_same_response <- function(student, answer) {
  stext <- deparse(student)
  if (length(answer) == 3) {
    # the answer has a response variable
    if (length(student) == 2) {
      return(paste0("Formula '", stext, "' lacks a response variable."))
    } else {
      # now we know they both have a LHS
      if (student[[2]] != answer[[2]])
        return(paste0("Formula '", stext,"' has wrong response variable."))
      else return("")
    }
  } else { # answer has only a RHS
    if (length(student) == 3) {
      return("There shouldn't be a response variable")
    }
  }

  return("")
}

# compare one term to a set of terms
# accounting for interactions
term_matches <- function(one, many) {
  # look for simple matches
  inds <- which(many == one)
  if (length(inds) > 0) return(inds[1])

  if (grepl(":", one)) {
    # look for interactions in <many>
    one_expanded <- sort(unlist(strsplit(one, split = ":")))
    for (k in 1:length(many)) {
      this <- sort(unlist(strsplit(many[k], split = ":")))
      if (length(this) == length(one_expanded) && all(one_expanded == this))
        return(k)
    }

  }

  integer(0) # return no match found
}
#' @export
f_same_explanatory <- function(student, answer) {
  # General tests on the explanatory side of the formula
  stext <- deparse(student) # the student's formula as written
  message = "" #value to be returned if no conflicts found

  # look for conflicts: extra or missing terms
  t1 <- attr(terms.formula(student), "term.labels")
  t2 <- attr(terms.formula(answer),  "term.labels")

  extra   <- rep(FALSE, length(t1))
  for (k in 1:length(t1)) {
    # work through student term-by-term
    ind <- term_matches(t1[k], t2) # expand the ":" as necessary
    if (length(ind) == 0) {
      extra[k] <- TRUE
      next
    } else {
      # get rid of matching term in answer
      t2 <- t2[ - ind[1]]
    }
  }
  extras <- t1[extra]
  missing <- t2

  message <- ""
  if (length(extras) + length(missing) > 0) {
    extra_text <- missing_text <- connector <- ""
    if(length(extras) > 0) {
      extra_text <- paste("has extras",
                          paste(paste0("'", extras, "'"), collapse = ", "))
    }
    if(length(missing) > 0) {
      missing_text <- paste("is missing",
                            paste(paste0("'", missing, "'"), collapse = ", "))
    }
    if (length(extras) > 0 && length(missing) > 0)
      connector <- "and"

    message <- paste("Formula", stext, missing_text, connector, extra_text)
  }

  res <- message
  attr(res, "extras") <- extras
  attr(res, "missing") <- missing

  res
}


