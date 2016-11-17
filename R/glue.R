#' glue to hold the test statements together
#'
#' @export
`%before%` <- function(first, second) {
  one <- first
  if (one$found) {
    one <- clobber_after(one, one$line)
    return(one$code %>% second)
  }
  else
    return(one$message)
}

#' @export
`%after%` <- function(first, second) {
  one <- first
  if (one$found) {
    one <- clobber_before(one, one$line)
    return(one$code %>% second)
  } else
    return(one$message)
}

#' @export
`%and%` <- function(first, second) {
  if (first$found) {
    return(first$code %>% second)
  } else
    return(first$message)
}

# Can't use %>% because we don't want <first> to be the
# first argument to functions like in_names(). Instead, we have to
# capture the value of in_names() (which will be a function) and pass <first>
# into that function.

#' @export
`%into%` <- function(first, second) {
  first %>% second
}

# Check whether message is empty.  If so, success! Move on to the next check.


#' @export
clobber_before <- function(code, k) {
  code$valid_lines <- k:length(code$returns)
  code
}
#' @export
clobber_after <- function(code, k) {
  code$valid_lines <- 1:(k-1)
  code
}
