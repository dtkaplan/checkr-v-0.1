context("Glue among tests")


example_1 <-
  "x <- 2
y <- x^2
lm(mpg ~ hp, data = mtcars)"

test_that("checks go in the right sequence", {
  one <- capture.code(example_1) %>%
    in_names("y", "match", "Did you define y?") %before%
    in_names("x", "match", "You need x before y")
  expect_equal(2 * 2, 4)
})
