context("grab_arguments")



test_that("multiplication works", {
  USER_CODE <- capture.code("1 + 3")
  SOLUTION_CODE <- capture.code("2 + 2")
  test_1 <- fcall("whatever + whatever")
  grab_me <- grab_argument("whatever + grab_this")
  USER_CODE %>% test_1 %>% grab_me -> foo

  # NEED to integrate line-finding into grab_argument.

  # NOT YET FIGURED OUT
  match_values(USER_CODE, SOLUTION_CODE,
               a = in_statements("+"),
               b = grab_me(in_statements("+")),
               same_num(b))
})
