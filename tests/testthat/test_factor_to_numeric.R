library(sportsanalyticsr)
context("factor_to_numeric test")
new_df <- data.frame(
  name = c("Tom", "Jolene", "Harriet", "Chon", "Sly"),
  age = c("16.3", "54.3", "32", NA, "25.01"),
  birth_month = c("3", "9", "2", "10", "5"))
test_that("factor_to_numeric returns either numeric values or NA", {
  expect_equal(
  factor_to_numeric(new_df, c("age", "birth_month"))$birth_month,
  c(3, 9, 2, 10, 5))

  expect_equal(
    factor_to_numeric(new_df, c("age", "birth_month"))$age,
    c(16.3, 54.3, 32, NA, 25.01))
})
