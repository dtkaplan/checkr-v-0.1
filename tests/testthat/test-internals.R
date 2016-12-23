context("test_internals")

test_that("removing pipe temporaries in printed text", {
  expect_equal(
    checkr:::kill_pipe_tmp_vars("..tmp1.. <- filter(..tmp3.., total > 50)"),
    "filter(total > 50)")
})
