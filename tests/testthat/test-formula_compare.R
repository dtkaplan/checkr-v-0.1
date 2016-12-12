context("Compare model formulas")

test_that("response variable is matched", {
  expect_equal(checkr:::f_same_response(wage ~ age + sex, wage ~ educ*sector), "")
  one <- checkr:::f_same_response(age ~ age + sex, wage ~ educ * sector)
  expect_true(one == "Formula 'age ~ age + sex' has wrong response variable.")
  two <- checkr:::f_same_response( ~ age, wage ~ age)
  expect_true(two == "Formula '~age' lacks a response variable.")
  })

test_that("Whole formula matches, term by term", {
  A <- wage ~ age * sector + educ * exper
  B <- wage ~ sector * age + exper * educ
  one <- match_formula(A)
  expect_true("" == one(B))
  B <- wage ~ sector + age + exper * educ
  two <- match_formula(A)
  expect_true("Formula wage ~ sector + age + exper * educ is missing 'age:sector'  " == two(B))
  three <- match_formula(B)
  expect_true(three(A) == "Formula wage ~ age * sector + educ * exper   has extras 'age:sector'")
})
