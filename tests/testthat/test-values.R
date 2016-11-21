context("Functions for getting values")



test_that("strict and non-strict name matching works", {
  one <- get_match_ind("mod", c("Mod", "M_od", "mod"), strict = TRUE)
  expect_equal(one, 3) # only # 3 matches
  two <- get_match_ind("mod", c("Mod", "M_od", "mod"), strict = FALSE)
  expect_equal(two, 1:3) # they all match
})
