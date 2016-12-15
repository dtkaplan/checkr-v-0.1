context("Compare object values to a standard ")

test_that("numerical comparison works", {
  expect_true("" == match_number(5)(5))
  expect_false("" == match_number(6, pm = 0.5)(5))
  expect_true("" == match_number(6, pm = 1.1)(5))
  expect_true("" == match_number(6, tol = 0.5)(5))
  expect_equal("gives wrong numerical value", match_number(6, tol = 0.01)(5))
  expect_equal("should be 6 plus or minus 0.01", match_number(6, tol = 0.01, hint = TRUE)(5))
})

test_that("class comparison works", {
  mod <- lm(mpg ~ hp, data = mtcars)
  expect_true("" == match_class("lm")(mod))
  expect_equal("has wrong class.", match_class("glm")(mod))
  expect_equal("should have class glm", match_class("glm", hint = TRUE)(mod))
})

test_that("data frame variable name comparison works", {
  small <- mtcars %>% head %>% select(hp, mpg)
  expect_equal("", match_data_frame(small)(mtcars))
  expect_equal("data frame missing variables.", match_data_frame(mtcars)(small))
  # show the names of the missing variables
  expect_true(grepl("'cyl', 'disp', 'drat'", match_data_frame(mtcars, hint = TRUE)(small)))

})

test_that("data frame nrow comparison works",{
  small <- mtcars %>% head %>% select(hp, mpg)
  expect_equal("", match_data_frame(mtcars, nrow = TRUE)(mtcars))
  expect_equal("data frame has wrong number of rows.", match_data_frame(small, nrow = TRUE)(mtcars))
  expect_equal("data frame has 32 rows but needs 6 rows.",
               match_data_frame(small, nrow = TRUE, hint = TRUE)(mtcars))
})

test_that("data frame class comparison works", {
  mtcars$name <- row.names(mtcars)
  small <- mtcars %>% head %>% select(hp, mpg, name)
  expect_equal("", match_data_frame(small, classes = TRUE)(mtcars))
  small$hp <- as.character(small$hp)
  expect_equal("data frame has wrong class for 'hp'.",
               match_data_frame(small, classes = TRUE)(mtcars))
  expect_equal("data frame variable 'hp' should be class character.",
               match_data_frame(small, classes = TRUE, hint = TRUE)(mtcars))

})

