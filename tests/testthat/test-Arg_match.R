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
  expect_true(one$passed)
  one <- example_1 %>%
    find_function("lm(data = mtcars)", "Use mtcars as data in lm()")
  expect_true(one$passed)
  one <- example_1 %>%
    find_function("lm(data = CPS85)", "Use CPS85 as data in lm()")
  expect_false(one$passed)

  one <- example_1 %>%
    find_function("lm(hp ~ mpg)", "check your formula.")
  expect_false(one$passed)

  })

test_that("Looking for mistakes works", {
  test_1 <- fcall("lm(hp ~ mpg)", mistake = TRUE, "Response should be 'mpg'.")
  one <- example_1 %>% test_1
  expect_true(one$passed)
  test_2 <- fcall("lm(mpg ~ hp)", mistake = TRUE,
                  "Well, ... not really a mistake, but just for testing purposes.")
  two <- example_1 %>% test_2
  expect_false(two$passed)
  test_3 <- fcall("lm(mpg ~ hp)", "This should pass")
  three <- example_1 %>% test_3
  expect_true(three$passed)
  expect_equal(three$line, 3) # should identify the line where the match was found
  test_4 <- fcall("lm()", "This should pass")
  four <- example_1 %>% test_4
  expect_true(four$passed)
  expect_equal(four$line, 3) # should identify the line where the match was found
  test_5 <- fcall("lm(mpg ~ whatever)", "This should pass")
  five <- example_1 %>% test_5
  expect_true(five$passed)
  expect_equal(five$line, 3) # should identify the line where the match was found
  test_6 <- fcall("lm(whatever ~ whatever)", "This should pass")
  six <- example_1 %>% test_6
  expect_true(six$passed)
  expect_equal(six$line, 3) # should identify the line where the match was found
  test_7 <- fcall("lm(data = mmmmcars)", "this should fail")
  seven <- example_1 %>% test_7
  expect_false(seven$passed)
  test_8 <- fcall("lm(data = whatever)", "this should pass")
  eight <- example_1 %>% test_8
  expect_true(eight$passed)
  expect_equal(eight$line, 3) # should identify the line where the match was found
  test_9 <- fcall("lm(mpg ~ whatever, data = whatever)", "this should pass")
  nine <- example_1 %>% test_9
  expect_true(nine$passed)
  expect_equal(nine$line, 3) # should identify the line where the match was found
})


