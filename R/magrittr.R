#' Expand a Magrittr chain so arguments resolve correctly
#'
#' @export
#'
my_test <- "foo <- mtcars %>% select(hp, mpg, carb) %>% filter(mpg > 15)"


#' @export
expand_chain <- function(chain_string) {
  components <- unlist(strsplit(chain_string, "%>%", fixed = TRUE))
  if (length(components) == 1) return(chain_string)

  # is there an assignment in the first component? Grab the variable name
  assigned_to <- stringr::str_match(components[1], "^( *[a-zA-Z0-9._]*) *<-")[2]
  if (is.na(assigned_to)) first_bit <- components[1]
  else components[1] <- stringr::str_match(components[1], "^.*<- *(.*) *$")[2]

  components[1] <- paste(components[1], "-> ..tmp1..")
  for (k in 2:length(components)) {
    components[k] <- insert_magrittr_input(components[k], sprintf("..tmp%d..", k-1))
    if (k == length(components) && !is.na(assigned_to)) {
      components[k] <- paste(components[k], sprintf("-> %s", assigned_to))
    } else {
      components[k] <- paste(components[k], sprintf("-> ..tmp%d..", k))
    }
  }
  paste0(components, collapse = "; ")
}

# insert the first bit explicitly in the second component
insert_magrittr_input <- function(command, input_name) {
  # simple for now

  # but ultimately, search for a dot and replace it with input_name
  res <- sub("(+[a-zA-Z0-9._]*\\()", sprintf("\\1%s, ", input_name), command)
  # get rid of any empty last argument
  gsub(", \\)", ")", res)

}

# FOLLOWING FUNCTION needs to be updated to integrae with expand_chain().

# is there a . argument to the function
# the_args is the as.list() version of the function call
is_there_a_dot <- function(the_args) {
  dot_yet <- FALSE
  for (k in 2:length(the_args)) {
    if (is.call(the_args[[k]])) {
      dot_yet <- dot_yet || is_there_a_dot(as.list(the_args[[k]]))
    } else {
      dot_yet <- dot_yet || (the_args[[k]] == as.name("."))
    }
    if (dot_yet) return(TRUE)
  }

  FALSE
}
