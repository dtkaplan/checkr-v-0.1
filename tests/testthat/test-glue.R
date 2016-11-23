context("Glue among tests")


example_1 <- capture.code(
" x <- 2
  y <- x^2
  lm(mpg ~ hp, data = mtcars)"
)

test_that("simple checks work", {
  test_1 <- in_names("x", "Did you define x?")
  one <- example_1 %>% test_1
  expect_true(one$passed)
  test_2 <- in_names("bogus", "no object named 'bogus' found")
  two <- example_1 %>% test_2
  expect_false(two$passed)
  expect_equal(two$message, "no object named 'bogus' found")
  test_3 <- in_names("bogus")
  three <- example_1 %>% test_3
  expect_false(three$passed)
  expect_equal(three$message, "couldn't find match to 'bogus'")
  test_4 <- in_values(4, number = TRUE)
  four <- example_1 %>% test_4
  expect_true(four$passed)
  test_5 <- in_values(7, number = TRUE)
  five <- example_1 %>% test_5
  expect_false(five$passed)
  expect_equal(five$message, "couldn't find match to number '7'")
})

test_that("checks go in the right sequence", {
  test_0 <- in_names("x", "Did you define x?")
  test_1 <- in_names("y", "Did you define y?")
  test_2 <- in_names("x", "You need x before y")
  test_3 <- in_names("bogus", "need to define 'bogus' before 'y'")
  one <- example_1 %>% test_0 %>% test_2 %>% then %>% test_1
  expect_true(one$passed)
  three <- example_1 %>% test_1 %>% previously %>% test_2
  expect_true(three$passed)
  four <- example_1 %>% test_1 %>% previously %>% test_3
  expect_false(four$passed)
  expect_equal(four$message, "need to define 'bogus' before 'y'")
  test_5 <- in_names("z", "Did you define z?")
  five <- example_1 %>% test_5
  expect_false(five$passed)
  expect_equal(five$message, "Did you define z?")
t})

test_that("Negative statements work.", {
  test_1 <- in_names("z", "match", "should not be a z variable", mistake = TRUE)
  one <- example_1 %>% test_1
  expect_true(one$passed)
})

test_that("Function tests work", {
  test_1 <- in_names("x", "where's x?")
  test_2 <- fcall("x^2", "you didn't square x")
  one <- example_1 %>% test_1 %>% then %>% test_2
  expect_true(one$passed)
  one <- example_1 %>% test_2 %>% then %>% test_1
  expect_false(one$passed)
  # testing whether specific mistake is found
  test_3 <- fcall("x^2", mistake = TRUE, "Why are you squaring x?")
  two <- example_1 %>%  test_3
  expect_false(two$passed)
  expect_equal(two$message, "Why are you squaring x?")
})

test_that("The either() function works", {
  test_1 <- in_names("z", "z not found")
  test_2 <- in_names("x", "x not found")
  test_3 <- in_names("w", "w not found")
  test_4 <- either(test_1, test_2)
  test_5 <- either(test_1, test_3) # should be wrong
  test_6 <- in_names("y2", "y2 not found")
  test_7 <- either(test_1, test_6, test_3) # should be wrong
  test_8 <- either(test_1, test_6, test_2, test_3) # should be true
  one <- example_1 %>% test_4
  expect_true(one$passed)
  two <- example_1 %>% test_5
  expect_false(two$passed)
  three <- example_1 %>% test_7
  expect_false(three$passed)
  four <- example_1 %>% test_8
  expect_true(four$passed)

})

