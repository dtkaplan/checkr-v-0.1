context("Can match values")

test_that("can carry out the match tests", {
  USER1_CODE <- capture.code("a <- 7\n b <- a^2")
  USER2_CODE <- capture.code("a <- 7\n x <- a^2")
  ANSWER1_CODE <- capture.code("a <- 7\nb <- a^2")
  ANSWER2_CODE <- capture.code("a <- 7\nb <- a^3")
  ANSWER3_CODE <- capture.code("a <- 7\n x <- a^2")
  expect_true(soln_test(USER1_CODE, ANSWER1_CODE,
                            a = assigns_to("a"),
                            b = assigns_to("b"),
                            d = a + b,
                            same_num(a),
                            same_num(b),
                            same_num(d))$passed)
  expect_true(grepl( "gives wrong numerical value",
               soln_test(USER1_CODE, ANSWER2_CODE,
                            a = assigns_to("a"),
                            b = assigns_to("b"),
                            d = a + b,
                            same_num(a),
                            same_num(b),
                            same_num(d, hint = TRUE))$message))
  expect_error(soln_test(USER1_CODE, ANSWER3_CODE,
                                  a = assigns_to("a"),
                                  b = assigns_to("b"),
                                  d = a + b,
                                  same_num(a),
                                  same_num(b),
                                  same_num(d)))

  expect_equal("I couldn't find a command that creates an object named 'b'.",
               soln_test(USER2_CODE, ANSWER1_CODE,
                            a = assigns_to("a"),
                            b = assigns_to("b"),
                            d = a + b,
                            same_num(a),
                            same_num(b),
                            same_num(d))$message)

})
