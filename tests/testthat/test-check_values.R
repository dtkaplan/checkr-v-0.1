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
  data(CPS85, package = "mosaicData")
  small <- CPS85 %>% head %>% select(wage, age)
  expect_equal("", match_data_frame(small)(CPS85))
  expect_equal("data frame missing variables.", match_data_frame(CPS85)(small))
  # show the names of the missing variables
  expect_true(grepl("'exper', 'union', 'sector'", match_data_frame(CPS85, hint = TRUE)(small)))

})

test_that("data frame nrow comparison works",{
  data(CPS85, package = "mosaicData")
  small <- CPS85 %>% head %>% select(wage, age)
  expect_equal("", match_data_frame(CPS85, nrow = TRUE)(CPS85))
  expect_equal("data frame has wrong number of rows.", match_data_frame(small, nrow = TRUE)(CPS85))
  expect_equal("data frame has 534 rows but needs 6 rows.",
               match_data_frame(small, nrow = TRUE, hint = TRUE)(CPS85))
})

test_that("data frame class comparison works", {
  data(CPS85, package = "mosaicData")
  small <- CPS85 %>% head %>% select(wage, age)
  expect_equal("", match_data_frame(small, classes = TRUE)(CPS85))
  small$wage <- as.character(small$wage)
  expect_equal("data frame has wrong class for 'wage'.",
               match_data_frame(small, classes = TRUE)(CPS85))
  expect_equal("data frame variable 'wage' should be class character.",
               match_data_frame(small, classes = TRUE, hint = TRUE)(CPS85))

})

