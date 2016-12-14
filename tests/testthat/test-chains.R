context("queries on magrittr chains")

library(dplyr)
chain_text_1 <- "mtcars %>% filter(cyl == 5) %>% arrange(hp)"
chain_text_2 <- "mtcars %>% group_by(cyl) %>% summarise(hp = mean(hp))"
chain_text_3 <- "mtcars %>% summarise(hp = mean(hp))"



test_that("Chains are properly broken up by capture.code()", {
  expect_equal(3, length(capture.code(chain_text_1)$statements))
  expect_equal(3, length(capture.code(chain_text_2)$statements))
  expect_equal(2, length(capture.code(chain_text_3)$statements))
})
