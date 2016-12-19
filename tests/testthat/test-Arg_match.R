context("Argument matching")


example_1 <- capture.code(
  " x <- 2
  y <- x^2
  lm(mpg ~ hp, data = mtcars)"
)

test_that("Looking for mistakes works", {
  test_1 <- find_call("lm(hp ~ mpg, data = whatever)", mistake = TRUE, "Response should be 'mpg'.")
  one <- example_1 %>% test_1
  expect_true(one$passed) # the mistake wasn't in the submitted code
  test_2 <- find_call("lm(mpg ~ hp, data = whatever)", mistake = TRUE,
                  "Well, ... not really a mistake, but just for testing purposes.")
  two <- example_1 %>% test_2
  expect_false(two$passed)
  test_3 <- find_call("lm(mpg ~ hp)", "This should pass")
  three <- example_1 %>% test_3
  expect_true(three$passed)
  expect_equal(three$line, 3) # should identify the line where the match was found
  test_4 <- find_call("lm()", "This should pass")
  four <- example_1 %>% test_4
  expect_true(four$passed)
  expect_equal(four$line, 3) # should identify the line where the match was found
  test_5 <- find_call("lm(mpg ~ whatever)", "This should pass")
  five <- example_1 %>% test_5
  expect_true(five$passed)
  expect_equal(five$line, 3) # should identify the line where the match was found
  test_6 <- find_call("lm(whatever ~ whatever)", "This should pass")
  six <- example_1 %>% test_6
  expect_true(six$passed)
  expect_equal(six$line, 3) # should identify the line where the match was found
  test_7 <- find_call("lm(data = mmmmcars)", "this should fail")
  seven <- example_1 %>% test_7
  expect_false(seven$passed)
  test_8 <- find_call("lm(data = whatever)", "this should pass")
  eight <- example_1 %>% test_8
  expect_true(eight$passed)
  expect_equal(eight$line, 3) # should identify the line where the match was found
  test_9 <- find_call("lm(mpg ~ whatever, data = whatever)", "this should pass")
  nine <- example_1 %>% test_9
  expect_true(nine$passed)
  expect_equal(nine$line, 3) # should identify the line where the match was found
})


