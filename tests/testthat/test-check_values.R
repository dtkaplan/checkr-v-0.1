context("Compare object values to a standard ")

test_that("numerical comparison works", {
  expect_true(check_number(5)(5))
  expect_false(check_number(6, pm = 0.5)(5))
  expect_true(check_number(6, pm = 1.1)(5))
  expect_true(check_number(6, tol = 0.5)(5))
  expect_false(check_number(6, tol = 0.01)(5))
})

test_that("data frame variable name comparison works", {
  data(CPS85, package = "mosaicData")
  small <- CPS85 %>% head %>% select(wage, age)
  expect_equal("", check_data_frame(small)(CPS85))
  expect_equal("Data frame missing variables.", check_data_frame(CPS85)(small))
  # show the names of the missing variables
  expect_true(grepl("'exper', 'union', 'sector'", check_data_frame(CPS85, diag = TRUE)(small)))

})

test_that("data frame nrow comparison works",{
  data(CPS85, package = "mosaicData")
  small <- CPS85 %>% head %>% select(wage, age)
  expect_equal("", check_data_frame(CPS85, nrow = TRUE)(CPS85))
  expect_equal("Data frame has wrong number of rows.", check_data_frame(small, nrow = TRUE)(CPS85))
  expect_equal("Data frame has 534 rows but needs 6 rows.",
               check_data_frame(small, nrow = TRUE, diag = TRUE)(CPS85))
})

test_that("data frame class comparison works", {
  data(CPS85, package = "mosaicData")
  small <- CPS85 %>% head %>% select(wage, age)
  expect_equal("", check_data_frame(small, classes = TRUE)(CPS85))
  small$wage <- as.character(small$wage)
  expect_equal("Data frame has wrong class for 'wage'.",
               check_data_frame(small, classes = TRUE)(CPS85))
  expect_equal("Data frame variable 'wage' should be class character.",
               check_data_frame(small, classes = TRUE, diag = TRUE)(CPS85))

})
