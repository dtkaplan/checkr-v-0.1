context("same")

## TODO: Rename context
## TODO: Add more tests

test_that("in_both() works", {
  env1 <- list(a = 1, b = 2)
  env2 <- list(a = 11, b = 22)
  env3 <- list(b = 22, c = 33)
  expect_true(checkr:::in_both('a', env1, env2)$passed)
  expect_false(checkr:::in_both('a', env1, env3)$passed)
})


# compare_classes() is picky. For more general use,
# you can use inherits() when creating the object and compare_vectors() on the result.
test_that("comparing classes works", {
  s <- "hello"
  int <- 2L
  expect_equal("has wrong class", checkr:::compare_classes(s, int))
  expect_equal("",checkr:::compare_classes(s, s))
  expect_equal("has wrong class", checkr:::compare_classes(int, pi))
  expect_equal("should have class 'numeric'", checkr:::compare_classes(int, pi, hint = TRUE))


}
)

test_that("comparing numbers works", {
  expect_equal("", checkr:::compare_numbers(0, 0))
  expect_equal("", checkr:::compare_numbers(1, 1.1, pm = 0.15))
  expect_equal("", checkr:::compare_numbers(0, 0, hint = TRUE))
  expect_equal("", checkr:::compare_numbers(1, 1.1, pm = 0.15, hint = TRUE))
  expect_equal("should be 1.3 plus or minus 0.15", checkr:::compare_numbers(1, 1.3, pm = 0.15, hint = TRUE))
  expect_equal("", checkr:::compare_numbers(c(1,2,3), c(1,2,3)))
  expect_equal("should be [1 2 4] plus or minus 1e-08",
               checkr:::compare_numbers(c(1,2,3), c(1,2,4), hint = TRUE))

})

test_that("comparing vectors works", {
  # These tests involve numbers, but compare_vectors() would typically be used
  # for strings, while compare_numbers() is used for numbers.
  expect_equal("has wrong length", checkr:::compare_vectors(1:3, 1:4))
  expect_equal("", checkr:::compare_vectors(1:3, 1:3))
  expect_equal("isn't an exact match", checkr:::compare_vectors(1:3, 3:1))
  expect_equal("", checkr:::compare_vectors(1:3, 3:1, same_order = FALSE))
})

test_that("same_() family works", {
  # For testing, create by hand the environments that would be created by
  # soln_test()
  S <- list(a = 1:5, b = (1:5)^2)
  R <- c(S, d = 7)
  test1 <- same_num(a)
  test2 <- same_num(a^2)
  test3 <- same_num(exp(d), hint = TRUE)
  expect_true(is.function(test))
  expect_true(test1(S, R)$passed)
  expect_true(test2(S, R)$passed)
  expect_false(test3(S, R)$passed)
})
