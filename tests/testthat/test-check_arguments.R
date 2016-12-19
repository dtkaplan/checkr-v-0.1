context("check argument matching")

library(ggplot2)
test_that("argument alignment works (internal)", {
  call_1 <- parse(text = "atan2(1, 2)")
  call_2 <- parse(text = "atan2(2, 1)")
  call_3 <- parse(text = "atan2(grab_this, grab_this)")
  call_4 <- parse(text = "ggplot(data = CPS85, aes(x = hp, y = mpg))")
  call_5 <- parse(text = "ggplot(data = CPS85, grab_this)")
  call_6 <- parse(text = "ggplot(data = CPS85, aes(x = grab_this, y = grab_this, color = NULL))")

  one <- corresponding_arguments(call_1, call_2)
  two <- corresponding_arguments(call_1, call_3)
  three <- corresponding_arguments(call_4, call_6)

  expect_equal(three$grabbed$x, as.name('hp'))
  expect_equal(three$grabbed$y, as.name('mpg'))

  # NEED TO PUT IN THE expect_equal() statements
})




test_that("can pull out an argument match from a line", {
  example_1 <- capture.code("x <- 3 + 1
                          y <- x ^ 2")
  get_x_line <- find_assignment("x")
  test_3 <- check_argument("grab_this + 1", arg_is(3))
  test_4 <- check_argument("grab_this + 1", arg_is(4))
  test_5 <- check_argument("grab_this + 1", arg_is(4, hint=TRUE))
  test_6 <- check_argument("grab_this + 1",
                           arg_is(test = match_number(4), hint = TRUE))
  three <- example_1 %>% get_x_line %>% test_3
  expect_true(three$passed)
  four <- example_1 %>% get_x_line %>% test_4
  expect_false(four$passed)
  expect_equal(four$message, "in function call _____ + 1 the argument has wrong value.")
  five <- example_1 %>% get_x_line %>% test_5
  expect_false(five$passed)
  expect_equal(five$message, "in function call _____ + 1 the argument should have value 4.")
  six <- example_1 %>% get_x_line %>% test_6
  expect_equal(six$message, "in function call _____ + 1 the argument gives wrong numerical value")
})
