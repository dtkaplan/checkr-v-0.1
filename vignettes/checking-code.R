## ----include = FALSE-----------------------------------------------------
library(ggplot2)
library(dplyr)
library(checkr)

## ------------------------------------------------------------------------
# three possible submissions to the problem
submission_1 <- "2 + 2" # correct
submission_2 <- "2 / 2" # right arguments but wrong function
submission_3 <- "3 + 2" # right function but wrong arguments

## ------------------------------------------------------------------------
library(checkr)
test_1 <- fcall("whatever + whatever", "need to use addition (+)")
test_2 <- fcall("2 + whatever", "first argument should be 2")
test_3 <- fcall("whatever + 2", "second argument should be 2")

## ----echo = FALSE--------------------------------------------------------
show_results <- function(test_output) {
  if (test_output$passed) return("Passed!")
  else return(paste("Sorry, but", test_output$message))
}

## ------------------------------------------------------------------------
capture.code(submission_1) %>% test_1 %>% show_results
capture.code(submission_2) %>% test_1 %>% show_results

## ------------------------------------------------------------------------
capture.code(submission_3) %>% test_1 %>% show_results

## ------------------------------------------------------------------------
capture.code(submission_3) %>% test_1 %>% test_2 %>% test_3 %>% show_results

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
test_1 <- fcall("aes(x = hp, y = whatever)", "variable 'hp' goes on the x axis")
test_2 <- fcall("aes(y = mpg, x = whatever)", "variable 'mpg' goes on the y axis")
test_3 <- fcall("geom_point()", "include a 'geom_point()' layer")
test_4 <- in_statements("mtcars") 
test_5 <- fcall("ggplot(data = whatever)", "no data handed to ggplot()")
test_6 <- check_argument("ggplot(data = grab_this)", test = match_class("data.frame"))
test_7 <- check_argument("ggplot(data = grab_this)", match_data_frame(mtcars))
test_8 <- check_argument("ggplot(data = grab_this)", 
                         match_data_frame(mtcars, diag = TRUE))
test_9 <- check_argument("ggplot(data = grab_this)",
                         match_data_frame(
                           mtcars %>% select(hp, mpg, carb), 
                           diag = TRUE))

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
test_1 <- in_values(class = "ggplot")
capture.code(submission_1) %>%
  test_1 %>%
  show_results

## ------------------------------------------------------------------------
test_2 <- in_values(class = "lattice")
capture.code(submission_1) %>%
  test_2 %>%
  show_results

## ------------------------------------------------------------------------
test_3 <- either(test_1, test_2)
capture.code(submission_1) %>%
  test_3 %>%
  show_results

## ------------------------------------------------------------------------
test_4 <- in_values(number = c(7.9, 8.1))
capture.code(submission_1) %>% test_4 %>%
  show_results

## ------------------------------------------------------------------------
submission_1 <- capture.code("seq(11, 31, by = 2)") # right value, but not what was asked
submission_2 <- capture.code("11 + 2*(0:10)") # right
submission_3 <- capture.code("11 + 2*(1:11)") # uses colon, but wrong result


## ------------------------------------------------------------------------
test_1 <- fcall("whatever : whatever", "you didn't use the colon operator")
test_2 <- check_value(match_vector(seq(11, 31, by = 2), diag = TRUE))

submission_1 %>% test_1 %>% test_2 %>% show_results
submission_2 %>% test_1 %>% test_2 %>% show_results
submission_3 %>% test_1 %>% test_2 %>% show_results

## ------------------------------------------------------------------------
submission_1 <- capture.code("lm(mpg ~ hp, data = mtcars)")  # wrong
submission_2 <- capture.code("lm(mpg ~ hp + wt, data = mtcars)") # right
submission_3 <- capture.code("lm(mpg ~ wt, data = mtcars)") # wrong

## ------------------------------------------------------------------------
test_1 <- fcall("lm(data = mtcars)", "use lm() on mtcars data")
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
SOLUTION_CODE <- capture.code("2 + 2")
match_values(USER_CODE, SOLUTION_CODE,
             res = in_statements(regex="2 *\\+"),
             same_num(res)) %>% 
  show_results

## ------------------------------------------------------------------------
USER_CODE <- capture.code("2 + -6")
SOLUTION_CODE <- capture.code("2 + 2")
match_values(USER_CODE, SOLUTION_CODE,
             res = in_statements(regex="2 *\\+"),
             same_num(abs(res))) %>% 
  show_results

## ----eval = FALSE--------------------------------------------------------
#  USER_CODE <- capture.code("mod <- lm(mpg ~ hp + carb, data = mtcars)")
#  SOLUTION_CODE <- capture.code("mod <- lm(mpg ~ hp * carb, data = mtcars)")
#  match_values(USER_CODE, SOLUTION_CODE,
#               res = in_names("mod"),
#               same_vec(coef(res))) %>%
#    show_results

## ----eval = FALSE--------------------------------------------------------
#  submission_1 <- capture.code("foobar <- mtcars %>% filter(mpg > 15)")
#  test_1 <- fcall("filter()", "should call filter()")
#  test_2 <- check_argument("mpg > grab_this", match_number(15))
#  test_2A <- check_argument("mpg > grab_this", match_number(16))
#  test_3 <- check_argument("filter(.data = grab_this)", match_data_frame(iris, diag = TRUE))
#  submission_1 %>% test_1 %>% test_2 %>% test_3 %>% show_results
#  submission_1 %>% test_1 %>% test_2A %>% test_3 %>% show_results
#  

## ----eval = FALSE--------------------------------------------------------
#  submission_2 <- capture.code("mtcars %>% filter(mpg > 15) %>% group_by(cyl) %>% summarise(mmpg = mean(mpg))")
#  test_4 <- fcall("group_by()")
#  test_5 <- check_argument("group_by(.data = whatever, grab_this)", function(x) x)
#  submission_2 %>% test_4 %>% test_5

