#' Simply create tests to evaluate a value
#'
#' Turn a logical expression into a test for check_() functions.
#'
#' @param expr an unquoted expression written in terms of 'x' that returns a TRUE/FALSE value.
#' @param message an optional failure message
#' @rdname agrees
#' @export
#'
agrees <- function(expr, message = "") {
  test <- substitute(expr)
  if (! is.call(test)) stop("argument expr must be unquoted R expression")
  if (message == "") message <- sprintf("doesn't satisfy test '%s'.", deparse(test))
  if (! "x" %in% all.vars(test)) stop("expr for agrees() must be written in terms of argument 'x'.")
  f <- function(x) {
    res <- eval(test)
    if (result_is_pass(res)) return("")
    else return(message)
  }
  f
}

result_is_pass <- function(res) {
  output <-
    if (is.character(res)) res == ""
    else if (is.logical(res)) res
    else stop("results of tests should be either character or logical")

  output
}
