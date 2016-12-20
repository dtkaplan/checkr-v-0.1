context("Glue among tests")

test_that("simple checks work", {
  example_1 <- capture.code(
    " x <- 2
    y <- x^2
    lm(mpg ~ hp, data = mtcars)"
  )

  test_1 <- find_assignment("x", message = "Did you define x?")
  one <- example_1 %>% test_1
  expect_true(one$passed)
  test_2 <- find_assignment("bogus", message = "no object named 'bogus' found")
  two <- example_1 %>% test_2
  expect_false(two$passed)
  expect_equal(two$message, "no object named 'bogus' found")
  test_3 <- find_assignment("bogus")
  three <- example_1 %>% test_3
  expect_false(three$passed)
  expect_equal(three$message, "didn't see the assignment the problem asked you to make")
  test_4 <- find_value(4)
  four <- example_1 %>% test_4
  expect_true(four$passed)
  test_5 <- find_value(7)
  five <- example_1 %>% test_5
  expect_false(five$passed)
  expect_equal(five$message, "no value created matching [1] 7")
})

test_that("checks go in the right sequence", {
  example_1 <- capture.code(
    " x <- 2
    y <- x^2
    lm(mpg ~ hp, data = mtcars)"
  )

  test_0 <- find_assignment("x", message = "Did you define x?")
  test_1 <- find_assignment("y", message = "Did you define y?")
  test_2 <- find_assignment("x", message = "You need x before y")
  test_3 <- find_assignment("bogus", message = "need to define 'bogus' before 'y'")
  one <- example_1 %>% test_0 %>% test_2 %>% then %>% test_1
  expect_true(one$passed)
  three <- example_1 %>% test_1 %>% previously %>% test_2
  expect_true(three$passed)
  four <- example_1 %>% test_1 %>% previously %>% test_3
  expect_false(four$passed)
  expect_equal(four$message, "need to define 'bogus' before 'y'")
  test_5 <- find_assignment("z", "Did you define z?")
  five <- example_1 %>% test_5
  expect_false(five$passed)
  expect_equal(five$message, "Did you define z?")
t})


test_that("Function tests work", {
  example_1 <- capture.code(
    " x <- 2
    y <- x^2
    lm(mpg ~ hp, data = mtcars)"
  )

  test_1 <- find_assignment("x", message = "where's x?")
  test_2 <- find_call("x^2", "you didn't square x")
  one <- example_1 %>% test_1 %>% then %>% test_2
  expect_true(one$passed)
  one <- example_1 %>% test_2 %>% then %>% test_1
  expect_false(one$passed)
  # testing whether specific mistake is found
  test_3 <- find_call("x^2")
  two <- example_1 %>%  test_3 %>% was_mistake("Why are you squaring x?")
  expect_false(two$passed)
  expect_equal(two$message, "Why are you squaring x?")
})

test_that("The any_test() function works", {
  example_1 <- capture.code(
    " x <- 2
    y <- x^2
    lm(mpg ~ hp, data = mtcars)"
  )

  test_1 <- find_assignment("z", "z not found")
  test_2 <- find_assignment("x", "x not found")
  test_3 <- find_assignment("w", "w not found")
  test_4 <- any_test(test_1, test_2)
  test_5 <- any_test(test_1, test_3) # should be wrong
  test_6 <- find_assignment("y2", "y2 not found")
  test_7 <- any_test(test_1, test_6, test_3) # should be wrong
  test_8 <- any_test(test_1, test_6, test_2, test_3) # should be true
  one <- example_1 %>% test_4
  expect_true(one$passed)
  two <- example_1 %>% test_5
  expect_false(two$passed)
  three <- example_1 %>% test_7
  expect_false(three$passed)
  four <- example_1 %>% test_8
  expect_true(four$passed)

})

test_that("The all_tests() function works", {
  example_1 <- capture.code(
    " x <- 2
    y <- x^2
    lm(mpg ~ hp, data = mtcars)"
  )

  test_1 <- find_assignment("z", "z not found")
  test_2 <- find_assignment("x", "x not found")
  test_3 <- find_assignment("w", "w not found")
  test_4 <- any_test(test_1, test_2)

  expect_true(example_1 %>% all_tests()() %>% .$passed)
  expect_false(example_1 %>% all_tests(test_1)() %>% .$passed)
  expect_true(example_1 %>% all_tests(test_2)() %>% .$passed)
  expect_false(example_1 %>% all_tests(test_1, test_2)() %>% .$passed)
  expect_true(example_1 %>% all_tests(test_2, test_4)() %>% .$passed)
  expect_true(example_1 %>% all_tests(test_2, any_test(test_1, test_2))() %>% .$passed)
})

test_that("branch_test() works", {
  example_1 <- capture.code(
    " x <- 2
    y <- x^2
    lm(mpg ~ hp, data = mtcars)"
  )

  test_0 <- find_assignment("w", "w not found")
  test_1 <- find_assignment("z", "z not found")
  test_2 <- find_assignment("x", "x not found")
  test_3 <- find_formula()
  test_4 <- find_formula(test = match_formula(hp ~ mpg)) # should fail
  expect_false(example_1 %>% test_1 %>% .$passed)
  expect_true(example_1 %>% test_2 %>% .$passed)
  expect_true(example_1 %>% branch_test(test_1, test_2, test_3)() %>% .$passed)
  expect_false(example_1 %>% branch_test(test_1, test_2, test_0)() %>% .$passed)
  expect_true(example_1 %>% branch_test(test_2, test_3, test_1)() %>% .$passed)
  expect_false(example_1 %>% branch_test(test_2, test_0, test_1)() %>% .$passed)
})

test_that("within_pipe() correctly identifies the extent of a pipe", {
  U1 <- capture.code("x <- 7\n mtcars %>% arrange(hp) %>% filter(hp > 10)\n iris %>% select(Sepal.Length, Sepal.Width) %>%
                     mutate(Sepal.Area = Sepal.Length * Sepal.Width) %>% summarise(marea <- mean(Sepal.Area))\n y <- 6")

  test_1 <- find_pipe_start("mtcars")
  test_2 <- find_pipe_start("iris")
  one <- U1 %>% test_1 %>% within_pipe
  two <- U1 %>% test_2 %>% within_pipe
})

test_that("was_mistake() works", {
  U1 <- capture.code("theta <- pi \n sin(theta)")
  test_1 <- find_call("sin()")
  expect_true(U1 %>% test_1 %>% .$passed)
  two <- U1 %>% test_1 %>% was_mistake
  expect_false(two$passed)
  three <- U1 %>% test_1 %>% was_mistake(message = "use cos() and not sin().")
  expect_false(three$passed)
  expect_equal("use cos() and not sin().", three$message)
})
