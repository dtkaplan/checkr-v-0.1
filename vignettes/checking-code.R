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
test_1 <- fcall("whatever + whatever", "need to use addition (+)")
test_2 <- fcall("2 + whatever", "first argument should be 2")
test_3 <- fcall("whatever + 2", "second argument should be 2")

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
test_1 <- fcall("sin(whatever)", "you didn't use sin().")
test_2 <- check_value(match_number(sin(10), "something's wrong with the sin() line."))
USER_CODE %>%
  test_1 %>% test_2 %>%
  show_results

## ------------------------------------------------------------------------
test_a <- fcall("whatever + whatever", "remember to use +") # regular location test
test_b <- check_argument("grab_this + whatever", 
                         match_number(17, tol = 0.1))
USER_CODE %>%
  test_a %>% test_b %>%
  show_results

