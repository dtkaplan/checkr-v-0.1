context("Glue among tests")


example_1 <- capture.code(
" x <- 2
  y <- x^2
  lm(mpg ~ hp, data = mtcars)"
)

test_that("checks go in the right sequence", {
  test_0 <- in_names("x", "match", "Did you define x?")
  test_1 <- in_names("y", "match", "Did you define y?")
  test_2 <- in_names("x", "match", "You need x before y")
  one <- example_1 %>% test_0 %>% test_1 %>% before %>% test_2
  expect_true(one$found)
  two <- example_1 %>% test_0 %>% test_2 %>% after %>% test_1
  expect_true(two$found)
  three <- example_1 %>% test_1 %>% after %>% test_2
  expect_false(three$found)
  expect_equal(three$message, "You need x before y")
  test_4 <- in_names("z", "match", "Did you define z?")
  four <- example_1 %>% test_4
  expect_false(four$found)
  expect_equal(four$message, "Did you define z?")
t})

test_that("Negative statements work.", {
  test_1 <- in_names("z", "match", "should not be a z variable", wrong = TRUE)
  one <- example_1 %>% test_1
  expect_true(one$found)
})

test_that("Function tests work", {
  test_1 <- in_names("x", "match", "where's x?")
  test_2 <- fun_test("x^2", "you didn't square x")
  one <- example_1 %>% test_1 %>% after %>% test_2
  expect_true(one$found)
  # testing whether specific mistake is found
  test_3 <- fun_test("x^2", mistake = TRUE, "Why are you squaring x?")
  two <- example_1 %>%  test_3
  expect_false(two$found)
  expect_equal(two$message, "Why are you squaring x?")
})

test_that("The either() function works", {
  test_1 <- in_names("z", "match", "z not found")
  test_2 <- in_names("x", "match", "x not found")
  test_3 <- either(test_1, test_2)
  one <- example_1 %>% test_3
})
