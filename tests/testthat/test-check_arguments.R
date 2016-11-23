context("check argument matching")

library(ggplot2)
test_that("argument alignment works", {
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
  match_both_args <- function(concordance) {
    if (length(concordance$missing) == 0) return("")
    return(paste("missing arguments",
                 paste(paste0("'", concordance$missing, "'"), collapse = ", ")))
  }

  get_x_line <- in_names("x")
  test_1 <- check_argument("3 + 1", match_both_args)
  test_2 <- check_argument("1 + 3", match_both_args)
  test_3 <- check_argument("grab_this + 1", arg_is(3))
  test_4 <- check_argument("grab_this + 1", arg_is(4))
  test_5 <- check_argument("grab_this + 1", arg_is(4, diag=TRUE))
  test_6 <- check_argument("grab_this + 1", arg_is(test = check_number(4, diag = TRUE)))
  one <- example_1 %>% get_x_line %>% test_1
  expect_true(one$passed)
  two <- example_1 %>% get_x_line %>% test_2
  expect_true(two$passed)
  three <- example_1 %>% get_x_line %>% test_3
  expect_true(three$passed)
  four <- example_1 %>% get_x_line %>% test_4
  expect_false(four$passed)
  expect_equal(four$message, "argument 1 has wrong value.")
  five <- example_1 %>% get_x_line %>% test_5
  expect_false(five$passed)
  expect_equal(five$message, "argument 1 should have value 4.")
  six <- example_1 %>% get_x_line %>% test_6
  expect_equal(six$message, "should be 4")
})
