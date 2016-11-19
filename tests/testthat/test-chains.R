context("queries on magrittr chains")

chain_text_1 <- "mtcars %>% filter(cyl == 5) %>% arrange(hp)"
chain_text_2 <- "mtcars %>% group_by(cyl) %>% summarise(hp = mean(hp))"
chain_text_3 <- "mtcars %>% summarise(hp = mean(hp))"

test_that("Can move along chain", {
  one <- functions_in_chain(chain_text_1,
                             "group_by()", "remember to group.")
  expect_true(grepl("remember to group", one))
  two <- functions_in_chain(chain_text_1,
                             "summarise()", "remember to calculate the groupwise means")
  expect_true(grepl("remember to calculate the groupwise means", two))
  three <-
    functions_in_chain(chain_text_2,
                        "group_by()", "need to group",
                        "summarise", "haven't summarized by groupwise means")
  expect_equal(three, "")
  four <- functions_in_chain(chain_text_3,
                             "group_by()", "remember to group")
  expect_true(grepl("remember to group", four))
  five <- functions_in_chain(chain_text_1,
                             "filter()", "use filter()",
                             "summarise()", "use summarise()")
  expect_true(grepl("but use summarise\\(\\) after", five))
  })

test_that("Can handle capture input", {
  test_1 <- chain_test("filter()", "use filter()",
                    "summarise()", "use summarise()")
  one <- capture.code(chain_text_1) %>% test_1
  expect_true(grepl("but use summarise\\(\\) after", one$message))

})
