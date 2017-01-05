## ----include = FALSE-----------------------------------------------------
library(ggplot2)
library(dplyr)
library(checkr)
library(magrittr)

## ------------------------------------------------------------------------
library(checkr)
test_1 <- find_call("whatever + whatever", "you need to use addition (+)")
test_2 <- find_call("2 + whatever", "the first argument should be 2")
test_3 <- find_call("whatever + 2", "the second argument should be 2")
test_4 <- check_value(agrees(x == 4), "the result should be 4")

## ----echo = FALSE--------------------------------------------------------
print.test_result <- print.capture <- function(test_output) {
  if (test_output$passed) cat("Passed!\n")
  else cat(paste("Sorry, but", test_output$message), "\n")
  return(invisible(test_output))
}

## ------------------------------------------------------------------------
capture.code("2 + 2") %>%
  test_1 %>% test_2 %>% test_3 %>% final_ %>% test_4 

## ------------------------------------------------------------------------
capture.code("`+`(2, 2)") %>%
  test_1 %>% test_2 %>% test_3 %>% final_ %>% test_4 
capture.code("(2 + 2)") %>%
  test_1 %>% test_2 %>% test_3 %>% final_ %>% test_4 

## ------------------------------------------------------------------------
capture.code("3 + 1") %>%
  test_1 %>% test_2 %>% test_3 %>% final_ %>% test_4 
capture.code("2 / 2") %>%
  test_1 %>% test_2 %>% test_3 %>% final_ %>% test_4 

## ------------------------------------------------------------------------
# this line isn't needed in an actual tutor document
USER_CODE <- capture.code("x <- 7 + 3\n sin(x)")

## ------------------------------------------------------------------------
test_1 <- find_call("sin(whatever)", "you didn't use sin().")
test_2 <- check_value(match_number(sin(10), 
                                   "something's wrong with the sin() line."))
USER_CODE %>%
  test_1 %>% test_2 

## ------------------------------------------------------------------------
test_a <- find_call("whatever + whatever", "remember to use +") # regular location test
test_b <- check_argument("grab_this + whatever", 
                         match_number(17, tol = 0.1))
USER_CODE %>%
  test_a %>% test_b

## ------------------------------------------------------------------------
USER_CODE <- capture.code("sin(pi / 2)") 
test_1 <- find_call("pi/2", "you need to compute pi / 2.")
test_2 <- find_call("sin()") # wrong, but common mistake
test_3 <- find_call("cos()")
USER_CODE %>% 
  test_1 %>% test_2 %>% 
  was_mistake(message = "the x-coordinate is given by the cosine function") %>% 
  test_3 %>% 
  final_ %>% check_value(match_number(cos(pi/2)))() 

## ------------------------------------------------------------------------
USER_CODE <- capture.code("xx <- sin(7)") # wrong in so many ways!
test_1 <- find_call("sqrt(whatever)", "use the sqrt() function.")
test_2 <- find_assignment("x")
test_3 <- check_argument("sqrt(grab_this)", match_number(7))

## ------------------------------------------------------------------------
USER_CODE %>% test_2 %>% test_1 %>% test_3 

## ------------------------------------------------------------------------
USER_CODE %>% test_1 %>% test_3 %>% test_2 

## ------------------------------------------------------------------------
USER_CODE <- capture.code("
ggplot(mtcars, aes(y = mpg, x = hp)) + 
    geom_point()" 
)

## ------------------------------------------------------------------------
test_1 <- find_value(match_class("ggplot"))
USER_CODE %>% test_1

## ----eval = FALSE--------------------------------------------------------
#  test_2 <- find_value(match_class("lattice"))
#  USER_CODE %>% test_2

## ------------------------------------------------------------------------
test_3 <- any_test(test_1, test_2)
USER_CODE %>% test_3 

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

## ------------------------------------------------------------------------
capture.code(submission_1) %>% 
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 
capture.code(submission_2) %>%
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 
capture.code(submission_3) %>% 
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 
capture.code(submission_4) %>%
  test_1 %>% test_2 %>% test_3 %>% test_4 %>% test_5 %>% test_6 %>% test_7 

## ------------------------------------------------------------------------
submission_1 <- capture.code("seq(11, 31, by = 2)") # right value, but not what was asked
submission_2 <- capture.code("11 + 2*(0:10)") # right
submission_3 <- capture.code("11 + 2*(1:11)") # uses colon, but wrong result

## ------------------------------------------------------------------------
test_1 <- find_call("whatever : whatever", "you didn't use the colon operator")
test_2 <- check_value(match_vector(seq(11, 31, by = 2), hint = TRUE))

submission_1 %>% test_1 %>% test_2 
submission_2 %>% test_1 %>% test_2 
submission_3 %>% test_1 %>% test_2 

## ------------------------------------------------------------------------
submission_1 <- capture.code("lm(mpg ~ hp, data = mtcars)")  # wrong
submission_2 <- capture.code("lm(mpg ~ hp + wt, data = mtcars)") # right
submission_3 <- capture.code("lm(mpg ~ wt, data = mtcars)") # wrong

## ------------------------------------------------------------------------
test_1 <- find_call("lm(data = mtcars)", "use lm() on mtcars data")
test_2 <- check_value(agrees('wt' %in% names(coef(x))), 
                      "what about the covariate wt?")
test_3 <- check_value(agrees(all(c("hp", "wt") %in% names(coef(x)))), 
                      'include both hp and the covariate as explanatory variables')
test_4 <- check_argument("lm(formula = grab_this)", match_formula(mpg ~ hp + wt))

## ----eval = FALSE--------------------------------------------------------
#  submission_1 %>% test_1 %>% test_2 %>% test_3
#  submission_2 %>% test_1 %>% test_2 %>% test_3
#  submission_3 %>% test_1 %>% test_2 %>% test_3
#  submission_1 %>% test_1 %>% test_4
#  submission_3 %>% test_1 %>% test_4

## ------------------------------------------------------------------------
# these values are provided by tutor
USER_CODE   <- capture.code("2 + 2 + 2")
SOLN_CODE   <- capture.code("2 + 2")

## ------------------------------------------------------------------------
soln_test(USER_CODE, SOLN_CODE,
             res = final_,
             same_num(res)) 

## ------------------------------------------------------------------------
checkr:::compare_length

## ------------------------------------------------------------------------
SOLN_CODE <- capture.code("
  a <- sqrt(14)
  b <- 7 + 4")

## ------------------------------------------------------------------------
USER_CODE <- capture.code("
  frist <- 4 + 7
  sqrt(14)")

## ----eval = FALSE--------------------------------------------------------
#  soln_test(USER_CODE, SOLN_CODE,
#            one = closest_to(a),
#            same_num(one, hint = TRUE),
#            two = closest_to(b),
#            same_num(b, hint = TRUE))

## ------------------------------------------------------------------------
USER_CODE <- capture.code("2 + -6")
SOLN_CODE <- capture.code("2 + 2")
soln_test(USER_CODE, SOLN_CODE,
             res = find_statement("2 *\\+", regex = TRUE),
             same_num(abs(res))) 

## ----eval = FALSE--------------------------------------------------------
#  USER_CODE <- capture.code("mod <- lm(mpg ~ hp + carb, data = mtcars)")
#  SOLN_CODE <- capture.code("mod <- lm(mpg ~ hp * carb, data = mtcars)")
#  soln_test(USER_CODE, SOLN_CODE,
#               res = find_assignment("mod"),
#               same_vec(coef(res)))

## ------------------------------------------------------------------------
library(dplyr)
submission_1 <- capture.code("foobar <- mtcars %>% filter(mpg > 15)")
test_1 <- find_call("filter()", "should call filter()")
# Check whether the right value is used in filter
test_2 <- check_argument("mpg > grab_this", match_number(15))
# A test that will fail
test_2A <- check_argument("mpg > grab_this", match_number(16))
test_3 <- check_argument("filter(.data = grab_this)", match_data_frame(mtcars))
# A test that will fail
test_3A <- check_argument("filter(.data = grab_this)", match_data_frame(iris, hint = TRUE))
submission_1 %>% test_1 %>% test_2 %>% test_3 # should pass
submission_1 %>% test_1 %>% test_2A # should fail
submission_1 %>% test_1 %>% test_2 %>% test_3A # should fail


## ------------------------------------------------------------------------
submission_2 <- capture.code("
  mtcars %>% 
    filter(mpg > 15) %>% 
    group_by(cyl) %>% 
    summarise(mmpg = mean(mpg))")
test_4 <- find_call("group_by()", "remember to group before summarizing.")
test_5 <- find_call("group_by(.data = whatever, cyl)", "break down the result by `cyl`")
test_6 <- find_call("summarise()", "use summarise() after grouping")
test_7 <- find_call("mean(mpg)", "calculate the mean of mpg for each group")
submission_2 %>% test_4 %>% test_5 %>%  then %>% test_6 %>% test_7

## ------------------------------------------------------------------------
submission_2 <- capture.code("
  mtcars %>% 
    filter(mpg > 15) %>% 
    summarise(mmpg = mean(mpg), cyl = mean(cyl)) %>%
    group_by(cyl)")

submission_2 %>% test_4 %>% test_5 %>%  then %>% test_6 %>% test_7

