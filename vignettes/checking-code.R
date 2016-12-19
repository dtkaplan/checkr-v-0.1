## ----include = FALSE-----------------------------------------------------
library(ggplot2)
library(dplyr)
library(checkr)
library(magrittr)

## ------------------------------------------------------------------------
# three possible submissions to the problem
submission_1 <- "2 + 2" # correct
submission_2 <- "2 / 2" # right arguments but wrong function
submission_3 <- "3 + 2" # right function but wrong arguments

## ------------------------------------------------------------------------
library(checkr)
test_1 <- find_call("whatever + whatever", "need to use addition (+)")
test_2 <- find_call("2 + whatever", "first argument should be 2")
test_3 <- find_call("whatever + 2", "second argument should be 2")

## ----echo = FALSE--------------------------------------------------------
show_results <- function(test_output) {
  if (test_output$passed) return("Passed!")
  else return(paste("Sorry, but", test_output$message))
}

## ------------------------------------------------------------------------
capture.code(submission_1) %>% 
  test_1 %>% 
  show_results
capture.code(submission_2) %>% 
  test_1 %>% 
  show_results

## ------------------------------------------------------------------------
capture.code(submission_3) %>% 
  test_1 %>% 
  show_results

## ------------------------------------------------------------------------
capture.code(submission_3) %>% 
  test_1 %>% test_2 %>% test_3 %>% 
  show_results

## ------------------------------------------------------------------------
capture.code("2 + 2 + 2") %>% 
  test_1 %>% test_2 %>% test_3 %>% 
  show_results

## ------------------------------------------------------------------------
# these values are provided by tutor
USER_CODE   <- capture.code("2 + 2 + 2")
SOLN_CODE   <- capture.code("2 + 2")

# the -check chunk contents would look like
soln_test(USER_CODE, SOLN_CODE,
             res = final_,
             same_num(res)) %>%
  show_results()

## ------------------------------------------------------------------------
USER_CODE <- capture.code("x <- 7 + 3\n sin(x)")
test_1 <- find_call("sin(whatever)", "you didn't use sin().")
test_2 <- check_value(match_number(sin(10), "something's wrong with the sin() line."))
USER_CODE %>%
  test_1 %>% test_2 %>%
  show_results

## ------------------------------------------------------------------------
test_a <- find_call("whatever + whatever", "remember to use +") # regular location test
test_b <- check_argument("grab_this + whatever", 
                         match_number(17, tol = 0.1))
USER_CODE %>%
  test_a %>% test_b %>%
  show_results

## ------------------------------------------------------------------------
USER_CODE <- capture.code("sin(pi / 2)") 
test_1 <- find_call("pi/2", "you need to compute pi / 2.")
test_2 <- find_call("sin()") # wrong, but common mistake
test_3 <- find_call("cos()")
USER_CODE %>% 
  test_1 %>% test_2 %>% 
  was_mistake(message = "the x-coordinate is given by the cosine function") %>% 
  test_3 %>% 
  final_ %>% check_value(match_number(cos(pi/2)))() %>%
  show_results()

## ------------------------------------------------------------------------
USER_CODE <- capture.code("xx <- sin(7)") # wrong in so many ways!
test_1 <- find_call("sqrt(whatever)", "use the sqrt() function.")
test_2 <- find_assignment("x")
test_3 <- check_argument("sqrt(grab_this)", match_number(7))

## ------------------------------------------------------------------------
USER_CODE %>% test_2 %>% test_1 %>% test_3 %>% show_results

## ------------------------------------------------------------------------
USER_CODE %>% test_1 %>% test_3 %>% test_2 %>% show_results

## ------------------------------------------------------------------------
submission_1 <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()" # Wrong!
submission_2 <- "ggplot(mtcars, aes(x = hp, y = mpg)) + geom_line()" # Wrong!
submission_3 <- "ggplot(mtcars, aes(y = mpg, x = hp)) + geom_point()" # right
submission_4 <- "ggplot(mtcars) + geom_point(aes(y = mpg, x = hp))" # also right
submission_5 <- c(
"my_cars <- mtcars",
"ggplot(my_cars, aes(y = mpg, x = hp)) + geom_point()")
submission_6 <- c(
"my_cars <- mtcars %>% select(mpg, hp)",
"ggplot(my_cars, aes(y = mpg, x = hp)) + geom_point()")

## ------------------------------------------------------------------------
library(ggplot2)
test_1 <- find_call("aes(x = hp, y = whatever)", "variable 'hp' goes on the x axis")
test_2 <- find_call("aes(y = mpg, x = whatever)", "variable 'mpg' goes on the y axis")
test_3 <- find_call("geom_point()", "include a 'geom_point()' layer")
test_4 <- find_statement("mtcars") 
test_5 <- find_call("ggplot(data = whatever)", "no data handed to ggplot()")
test_6 <- check_argument("ggplot(data = grab_this)", test = match_class("data.frame"))
test_7 <- check_argument("ggplot(data = grab_this)", match_data_frame(mtcars))
test_8 <- check_argument("ggplot(data = grab_this)", 
                         match_data_frame(mtcars, hint = TRUE))
test_9 <- check_argument("ggplot(data = grab_this)",
                         match_data_frame(
                           mtcars %>% select(hp, mpg, carb), 
                           hint = TRUE))

## ------------------------------------------------------------------------
capture.code(submission_1) %>% 
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 %>% 
  show_results
capture.code(submission_2) %>%
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 %>%
  show_results
capture.code(submission_3) %>% 
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 %>% 
  show_results
capture.code(submission_4) %>%
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 %>%
  show_results

## ------------------------------------------------------------------------
capture.code(submission_5) %>%
  test_5 %>% test_7 %>%
  show_results
capture.code(submission_6) %>%
  test_5 %>% test_7 %>%
  show_results
capture.code(submission_6) %>%
  test_5 %>% test_8 %>%
  show_results
capture.code(submission_6) %>%
  test_5 %>% test_9 %>%
  show_results


## ------------------------------------------------------------------------
submission_1 <- "
3 + 5
ggplot(mtcars, aes(y = mpg, x = hp)) + geom_point()" 

## ------------------------------------------------------------------------
test_1 <- in_values(match_class("ggplot"))
capture.code(submission_1) %>%
  test_1 %>%
  show_results

## ----eval = FALSE--------------------------------------------------------
#  test_2 <- in_values(match_class("lattice"))
#  capture.code(submission_1) %>%
#    test_2 %>%
#    show_results

## ------------------------------------------------------------------------
test_3 <- any_test(test_1, test_2)
capture.code(submission_1) %>%
  test_3 %>%
  show_results

## ------------------------------------------------------------------------
test_4 <- in_values(match_number(8, range = c(7.9, 8.1)))
capture.code(submission_1) %>% test_4 %>%
  show_results

## ------------------------------------------------------------------------
submission_1 <- capture.code("seq(11, 31, by = 2)") # right value, but not what was asked
submission_2 <- capture.code("11 + 2*(0:10)") # right
submission_3 <- capture.code("11 + 2*(1:11)") # uses colon, but wrong result


## ------------------------------------------------------------------------
test_1 <- find_call("whatever : whatever", "you didn't use the colon operator")
test_2 <- check_value(match_vector(seq(11, 31, by = 2), hint = TRUE))

submission_1 %>% test_1 %>% test_2 %>% show_results
submission_2 %>% test_1 %>% test_2 %>% show_results
submission_3 %>% test_1 %>% test_2 %>% show_results

## ------------------------------------------------------------------------
submission_1 <- capture.code("lm(mpg ~ hp, data = mtcars)")  # wrong
submission_2 <- capture.code("lm(mpg ~ hp + wt, data = mtcars)") # right
submission_3 <- capture.code("lm(mpg ~ wt, data = mtcars)") # wrong

## ------------------------------------------------------------------------
test_1 <- find_call("lm(data = mtcars)", "use lm() on mtcars data")
test_2 <- check_value(function(x) {'wt' %in% names(coef(x))}, 
                      "what about the covariate wt?")
test_3 <- check_value(function(x) {all(c("hp", "wt") %in% names(coef(x)))}, 
                      'include both hp and the covariate as explanatory variables')
test_4 <- check_argument("lm(formula = grab_this)", match_formula(mpg ~ hp + wt))

## ----eval = FALSE--------------------------------------------------------
#  submission_1 %>% test_1 %>% test_2 %>% test_3 %>% show_results
#  submission_2 %>% test_1 %>% test_2 %>% test_3 %>% show_results
#  submission_3 %>% test_1 %>% test_2 %>% test_3 %>% show_results
#  submission_1 %>% test_1 %>% test_4 %>% show_results
#  submission_3 %>% test_1 %>% test_4 %>% show_results

## ------------------------------------------------------------------------
USER_CODE <- capture.code("2 + 3")
SOLN_CODE <- capture.code("2 + 2")
soln_test(USER_CODE, SOLN_CODE,
             res = find_statement("2 *\\+", regex = TRUE),
             same_num(res)) %>% 
  show_results

## ------------------------------------------------------------------------
USER_CODE <- capture.code("2 + -6")
SOLN_CODE <- capture.code("2 + 2")
soln_test(USER_CODE, SOLN_CODE,
             res = find_statement("2 *\\+", regex = TRUE),
             same_num(abs(res))) %>% 
  show_results

## ----eval = FALSE--------------------------------------------------------
#  USER_CODE <- capture.code("mod <- lm(mpg ~ hp + carb, data = mtcars)")
#  SOLN_CODE <- capture.code("mod <- lm(mpg ~ hp * carb, data = mtcars)")
#  soln_test(USER_CODE, SOLN_CODE,
#               res = find_assignment("mod"),
#               same_vec(coef(res))) %>%
#    show_results

## ----eval = FALSE--------------------------------------------------------
#  submission_1 <- capture.code("foobar <- mtcars %>% filter(mpg > 15)")
#  test_1 <- find_call("filter()", "should call filter()")
#  test_2 <- check_argument("mpg > grab_this", match_number(15))
#  test_2A <- check_argument("mpg > grab_this", match_number(16))
#  test_3 <- check_argument("filter(.data = grab_this)", match_data_frame(iris, hint = TRUE))
#  submission_1 %>% test_1 %>% test_2 %>% test_3 %>% show_results
#  submission_1 %>% test_1 %>% test_2A %>% test_3 %>% show_results
#  

## ------------------------------------------------------------------------
submission_2 <- capture.code("mtcars %>% filter(mpg > 15) %>% group_by(cyl) %>% summarise(mmpg = mean(mpg))")
test_4 <- find_call("group_by()")
test_5 <- check_argument("group_by(.data = whatever, grab_this)", function(x) x)
submission_2 %>% test_4 %>% test_5 %>% show_results

