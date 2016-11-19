context("Compare model formulas")

test_that("response variable is matched", {
  expect_equal(f_same_response(wage ~ age + sex, wage ~ educ*sector), "")
  one <- f_same_response(age ~ age + sex, wage ~ educ * sector)
  expect_true(one == "Formula 'age ~ age + sex' has wrong response variable.")
  two <- f_same_response( ~ age, wage ~ age)
  expect_true(two == "Formula '~age' lacks a response variable.")
  })

test_that("Whole formula matches, term by term", {
  A <- wage ~ age * sector + educ * exper
  B <- wage ~ sector * age + exper * educ
  one <- compare_model_formulas(A, B)
  expect_true(one == "")
  B <- wage ~ sector + age + exper * educ
  two <- compare_model_formulas(A, B)
  expect_true(two == "Formula wage ~ age * sector + educ * exper   has extras 'age:sector'")
  three <- compare_model_formulas(B, A)
  expect_true(three == "Formula wage ~ sector + age + exper * educ is missing 'age:sector'  ")
})
