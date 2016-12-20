context("pattern matching for lines in code")

test_that("final_() works", {
  USER_CODE_1 <- capture.code("7 + 3\n 1:10")
  SOLN_CODE_1 <- capture.code("1:10")
  SOLN_CODE_2 <- capture.code("10:1")
  SOLN_CODE_3 <- capture.code("b <- 1:10")
  expect_true(soln_test(USER_CODE_1, SOLN_CODE_1,
               val = final_,
               same_num(sort(val)))$passed)
  expect_true(soln_test(USER_CODE_1, SOLN_CODE_2,
                           val = final_,
                           same_num(sort(val)))$passed)
  expect_false(soln_test(USER_CODE_1, SOLN_CODE_2,
                           val = final_,
                           same_num(val, hint = TRUE))$passed)
  expect_true(soln_test(USER_CODE_1, SOLN_CODE_3,
                           val = final_,
                           same_num(sort(val)))$passed)
})

test_that("find_the_closest_one() works", {
  USER_CODE_1 <- capture.code("7 + 3\n 1:10\n diag(4)\n lm(hp ~ mpg, data = mtcars)")
  # find_the_closest_one() returns a line value
  expect_equal(1, checkr:::find_the_closest_one(USER_CODE_1, 5))
  expect_equal(2, checkr:::find_the_closest_one(USER_CODE_1, 1:3))

  expect_equal(3, checkr:::find_the_closest_one(USER_CODE_1, matrix(1:4, nrow = 2)))
  expect_equal(4, checkr:::find_the_closest_one(
                 USER_CODE_1,
                 glm(hp > 3 ~ cyl, data = mtcars, family = 'binomial')))
    })

test_that("closest_to() works", {
  USER_CODE_1 <- capture.code("7 + 3\n 1:10\n diag(4)\n lm(hp ~ mpg, data = mtcars)")
  SOLN_CODE_1 <- capture.code("1:9\n 15\n glm(hp>10 ~ mpg, data = mtcars)")
  one <- closest_to("1:9")
  a <- SOLN_CODE_1 %>% one
  expect_equal(1, a$line)
  # renew value so that we're seeing which line comes up
  one <- closest_to("1:9")

  expect_equal(2, one(USER_CODE_1)$line)
  expect_error(closest_to("lm(hp > 10 ~ mpg"))  # MUST be a parsable statement
  two <- closest_to("lm(hp > 10 ~ mpg)")
  expect_equal(3, two(SOLN_CODE_1)$line)
  expect_equal(4, two(USER_CODE_1)$line)
})

test_that("find_constants() works", {
  U <- capture.code("ifelse(sin(37 + 14) > .5, 'yes', 'no way')")
  test_1 <- find_constants(37, 14)
  test_2 <- find_constants("yes", "no way")
  test_3 <- find_constants(14, "no way", "yes", 37)
  one <- U %>% test_1 %>% test_2 %>% test_3
  expect_true(one$passed)
  U2 <- capture.code("ifelse(sin(73 + 14) > .5, 'yes', 'nope')")
  expect_false(U2 %>% test_1 %>% .$passed)
  expect_false(U2 %>% test_2 %>% .$passed)
  expect_false(U2 %>% test_1 %>% test_3 %>% .$passed)
  expect_false(U2 %>% test_3 %>% .$passed)
})

test_that("find_names() works", {
  U <-  capture.code("ifelse(sin(37 + 14) > .5, 'yes', 'no way')")
  test_1 <- checkr::find_names(ifelse(3 > sin(6), 2, 3))
  test_2 <- checkr::find_names(sin(x))
  test_3 <- checkr::find_names(sin(3))
  expect_true(U %>% test_1 %>% .$passed)
  expect_false(U %>% test_2 %>% .$passed)
  expect_true(U %>% test_3 %>% .$passed)
})

test_that("find_formula() works", {
  U1 <- capture.code("x <- sin(sqrt(3))\n y <- cos(x)")
  U2 <- capture.code("mod <- lm(mpg ~ hp, data = mtcars)")
  test_1 <- find_formula()
  test_2 <- find_formula(test = match_formula(mpg ~ hp))
  test_3 <- find_formula(test = match_formula(hp ~ mpg))
  expect_false(U1 %>% test_1 %>% .$passed)
  expect_true(U2 %>% test_1 %>% .$passed)
  expect_true(U2 %>% test_2 %>% .$passed)
  expect_false(U2 %>% test_3 %>% .$passed)
  U3 <- capture.code("mod <- lm(mpg ~ hp, data = mtcars)\n mod_2 <- lm(hp ~ mpg, data = mtcars)")
  three <- U3 %>% test_3
  expect_true(three$line == 2)
  four <- U3 %>% test_2
  expect_true(four$line == 1)
  })
