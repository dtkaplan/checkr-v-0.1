context("Tests on pipes")

test_that("find_pipe_start() works", {
  library(dplyr)
  U1 <- capture.code("x <- 7\nresult<- mtcars %>% summarise(mean_hp = mean(hp))")
  test_1 <- find_pipe_start("mtcars")
  one <- U1 %>% test_1
  expect_true(one$passed)
  expect_equal(2, one$line)
  U2 <- capture.code("w <- sqrt(3)\ny <- 19\nsqrt( 1: 20) %>% head\nz <- 46")
  test_2 <- find_pipe_start("sqrt(1:20)")
  two <- U2 %>% test_2
  expect_true(two$passed)
  expect_equal(3, two$line)
})

test_that("check_pipe_input() works", {
  U1 <- capture.code("x <- 7\nresult<- mtcars %>% summarise(mean_hp = mean(hp)) %>% arrange(mean_hp)")
  test_1 <- find_pipe_start("mtcars")
  test_2 <- fcall("summarise()", "can't find summarise() step in the pipe.")
  in_test <- check_pipe_input(test = match_data_frame(mtcars), message = "start the pipe with 'mtcars'.")
  out_test <- check_pipe_output(test = function(x) ifelse(nrow(x) == 1, "", nrow(x)), "should have only one row")
  out_test_2 <- check_pipe_output(test = match_data_frame(mtcars), message = "start the pipe with 'mtcars'.")
  in_test_2 <- check_pipe_input(test = function(x) ifelse(nrow(x) == 1, "", nrow(x)), "should have only one row")

  expect_true(U1 %>% test_1 %>% within_pipe %>% test_2 %>% in_test %>% .$passed)
  expect_true(U1 %>% test_1 %>% within_pipe %>% test_2 %>% out_test %>% .$passed)
  expect_false(U1 %>% test_1 %>% within_pipe %>% test_2 %>% out_test_2 %>% .$passed)
  expect_false(U1 %>% test_1 %>% within_pipe %>% test_2 %>% in_test_2 %>% .$passed)
})
