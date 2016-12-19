context("grab_arguments")



test_that("in_statements() works", {
  USER_CODE <- capture.code("1 + 3")
  SOLN_CODE <- capture.code("2 + 2")
  test_1 <- find_call("whatever + whatever")
  test_2 <- check_argument("whatever + grab_this", match_number(2))
  one <- USER_CODE %>% test_1 %>% test_2
  expect_false(one$passed)
  # check the value, not the argument
  two <- soln_test(USER_CODE, SOLN_CODE,
               a = in_statements("+"),
               same_num(a))
  expect_true(two$passed)
})
