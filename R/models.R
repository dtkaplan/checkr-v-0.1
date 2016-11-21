#' Check components of models
#'
#'
#' Try to find a model object that matches the formula
#' @param df a data frame that the model should have been fitted to
#' @param soln_formula a formula that the model should have
#' @export
check_model_formula <- function(soln_formula, message = "Give a real message") {
  f <- function(capture) {
    passed <- FALSE
    mod_count <- 0
    res <- message
    for (k in capture$valid_lines) {
      if (inherits(R[[k]], c("lm", "glm", "rpart"))) {
        mod_formula <- as.list(R[[k]]$call)$formula
        mod_count <- mod_count + 1
        res <- compare_model_formulas(mod_formula, soln_formula)
        if (res == "") {
          capture$passed <- TRUE
          capture$line <- k
          return(capture)
        }
      }
    }
    capture$passed <- FALSE
    if (mod_count == 1) {
      capture$message <- res
    } else {
      capture$message <- message
    }

    capture
  }
  f
}

check_model_data <- function(df, message = "Give a real message", ...) {
  g <- check_data_frame(df, ...)
  f <- function(capture) {
    passed <- FALSE
    mod_count <- 0
    res <- message
    for (k in capture$valid_lines) {
      if (inherits(R[[k]], c("lm", "glm", "rpart"))) {
        mod_data <- as.list(R[[k]]$call)$data
        res <- g(capture, df)
        if (res == "") {
          capture$passed <- TRUE
          capture$line <- k
          return(capture)
        }
      }
    }
    capture$passed <- FALSE
    if (mod_count == 1) {
      capture$message <- res
    } else {
      capture$message <- message
    }

    capture
  }
  f
}
