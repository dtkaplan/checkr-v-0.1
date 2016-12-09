context("Can match values")

USER1_CODE <- capture.code("a <- 7\n b <- a^2")
USER2_CODE <- capture.code("a <- 7\n x <- a^2")
ANSWER1_CODE <- capture.code("a <- 7\nb <- a^2")
ANSWER2_CODE <- capture.code("a <- 7\nb <- a^3")

test1 <- in_names("a")
roo <- V(test1)
roo(USER_CODE)


test_that("can carry out the match tests", {
  expect_equal("",
               match_values(USER1_CODE, ANSWER1_CODE,
                            a = in_names("a"),
                            b = in_names("b"),
                            d = a + b,
                            same_num(a),
                            same_num(b),
                            same_num(d)))
  expect_equal("",
               match_values(USER1_CODE, ANSWER3_CODE,
                            a = in_names("a"),
                            b = in_names("b"),
                            d = a + b,
                            same_num(a),
                            same_num(b),
                            same_num(d)))

  expect_equal("Couldn't find a command that creates an object named 'b'.",
               match_values(USER2_CODE, ANSWER1_CODE,
                            a = in_names("a"),
                            b = in_names("b"),
                            d = a + b,
                            same_num(a),
                            same_num(b),
                            same_num(d)))

})
