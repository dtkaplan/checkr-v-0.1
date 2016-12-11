context("pattern matching for lines in code")

test_that("final_() works", {
  USER_CODE_1 <- capture.code("7 + 3\n 1:10")
  SOLN_CODE_1 <- capture.code("1:10")
  SOLN_CODE_2 <- capture.code("10:1")
  SOLN_CODE_3 <- capture.code("b <- 1:10")
  expect_true(match_values(USER_CODE_1, SOLN_CODE_1,
               val = final_(),
               same_num(sort(val)))$passed)
  expect_true(match_values(USER_CODE_1, SOLN_CODE_2,
                           val = final_(),
                           same_num(sort(val)))$passed)
  expect_false(match_values(USER_CODE_1, SOLN_CODE_2,
                           val = final_(),
                           same_num(val, hint = TRUE))$passed)
  expect_true(match_values(USER_CODE_1, SOLN_CODE_3,
                           val = final_(),
                           same_num(sort(val)))$passed)
})

testthat("find_the_closest_one() works", {
  USER_CODE_1 <- capture.code("7 + 3\n 1:10\n diag(4)\n lm(hp ~ mpg, data = mtcars)")
  expect_equal(10, checkr:::find_the_closest_one(USER_CODE_1, 5))
  expect_equal(1:10, checkr:::find_the_closest_one(USER_CODE_1, 1:3))
  expect_equal(diag(4), checkr:::find_the_closest_one(USER_CODE_1, matrix(1:4, nrow = 2)))
  expect_equal("lm",
               class(checkr:::find_the_closest_one(
                 USER_CODE_1,
                 glm(hp > 3 ~ cyl, data = mtcars, family = 'binomial'))))
    })

testthat("closest_to() works", {
  USER_CODE_1 <- capture.code("7 + 3\n 1:10\n diag(4)\n lm(hp ~ mpg, data = mtcars)")
  SOLN_CODE_1 <- capture.code("1:9\n 15\n glm(hp>10 ~ mpg, data = mtcars)")
  one <- closest_to(":9")
  expect_equal(1, one(SOLN_CODE_1)$line)
  expect_equal(2, one(USER_CODE_1)$line)
  two <- closest_to("lm(hp > 10 ~ mpg") # MUST be a parsable statement
  expect_equal(3, two(SOLN_CODE_1)$line)
  expect_equal(4, two(USER_CODE_1)$line)
})
