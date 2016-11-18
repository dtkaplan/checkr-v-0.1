context("Argument matching")

test_that("match_the_arguments works with named args", {
  actual <- as.list(parse(text = "aes(x = hp, y = mpg)")[[1]])
  desired <- as.list(parse(text = "aes(y = mpg, x = hp)")[[1]])
  expect_true(match_the_arguments(actual, desired))
  actual <- as.list(parse(text = "aes(x = hp, y = mpg)")[[1]])
  desired <- as.list(parse(text = "aes(mpg, hp)")[[1]])
  expect_true(match_the_arguments(actual, desired))
  actual <- as.list(parse(text = "aes(hp, mpg)")[[1]])
  desired <- as.list(parse(text = "aes(x = mpg, y = hp)")[[1]])
  expect_true(match_the_arguments(actual, desired))
  actual <- as.list(parse(text = "aes(hpg, mpg)")[[1]])
  desired <- as.list(parse(text = "aes(x = mpg, y = NULL)")[[1]])
  expect_true(match_the_arguments(actual, desired))
  actual <- as.list(parse(text = "aes(hpg, hp)")[[1]])
  desired <- as.list(parse(text = "aes(x = mpg, y = NULL)")[[1]])
  expect_false(match_the_arguments(actual, desired)) # should be FALSE
})

test_that("match_the_arguments works with unnamed args", {
  actual <- as.list(parse(text = "2 + 2")[[1]])
  desired <- as.list(parse(text = "2 + 2")[[1]])
  expect_true(match_the_arguments(actual, desired))
  actual <- as.list(parse(text = "1 + 3")[[1]])
  desired <- as.list(parse(text = "3 + 1")[[1]])
  expect_true(match_the_arguments(actual, desired))
  desired <- as.list(parse(text = "NULL + NULL")[[1]])
  expect_true(match_the_arguments(actual, desired))
  desired <- as.list(parse(text = "NULL + 3")[[1]])
  expect_true(match_the_arguments(actual, desired))
  desired <- as.list(parse(text = "3 + NULL")[[1]])
  expect_true(match_the_arguments(actual, desired))
})

example_1 <- capture.code(
  " x <- 2
  y <- x^2
  lm(mpg ~ hp, data = mtcars)"
)

test_that("Looking for functions and arguments works", {
  one <- example_1 %>%
    find_function("lm(mpg ~ hp, data = mtcars)", "Wrong model formula in lm()")
  expect_true(one$found)
  one <- example_1 %>%
    find_function("lm(data = mtcars)", "Use mtcars as data in lm()")
  expect_true(one$found)
  one <- example_1 %>%
    find_function("lm(data = CPS85)", "Use CPS85 as data in lm()")
  expect_false(one$found)

  one <- example_1 %>%
    find_function("lm(hp ~ mpg)", "check your formula.")
  expect_false(one$found)

  })

test_that("Looking for mistakes works", {
  one <- example_1 %>%
    find_function("lm(hp ~ mpg)", mistake = TRUE, "Check your response variable.")
  expect_false(one$found)

  })

