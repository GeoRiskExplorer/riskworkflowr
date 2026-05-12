test_that("risk_distinct_category returns expected output columns", {

  x <- data.frame(
    unit_id = c("A", "A", "B", "B"),
    category = c("Falls", "Water", "Falls", "Water"),
    event_count = c(10, 2, 3, 8),
    exposure = c(1000, 1000, 800, 800)
  )

  result <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_equal(nrow(result), 2)
  expect_true("highest_category" %in% names(result))
  expect_true("highest_smr" %in% names(result))
  expect_true("highest_event_count" %in% names(result))
  expect_true("lowest_category" %in% names(result))
  expect_true("lowest_smr" %in% names(result))
  expect_true("lowest_event_count" %in% names(result))
  expect_true("category_count_used" %in% names(result))
  expect_true("insufficient_count_flag" %in% names(result))

})

test_that("risk_distinct_category identifies highest category", {

  x <- data.frame(
    unit_id = c("A", "A", "B", "B"),
    category = c("Falls", "Water", "Falls", "Water"),
    event_count = c(10, 2, 3, 8),
    exposure = c(1000, 1000, 800, 800)
  )

  result <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  a <- result[result$unit_id == "A", ]
  b <- result[result$unit_id == "B", ]

  expect_equal(a$highest_category, "Falls")
  expect_equal(b$highest_category, "Water")

})

test_that("risk_distinct_category respects min_count", {

  x <- data.frame(
    unit_id = c("A", "A", "B", "B"),
    category = c("Falls", "Water", "Falls", "Water"),
    event_count = c(10, 2, 3, 8),
    exposure = c(1000, 1000, 800, 800)
  )

  result <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure",
    min_count = 9
  )

  b <- result[result$unit_id == "B", ]

  expect_true(is.na(b$highest_category))
  expect_equal(b$category_count_used, 0)
  expect_true(b$insufficient_count_flag)

})

test_that("risk_distinct_category can omit lowest category", {

  x <- data.frame(
    unit_id = c("A", "A", "B", "B"),
    category = c("Falls", "Water", "Falls", "Water"),
    event_count = c(10, 2, 3, 8),
    exposure = c(1000, 1000, 800, 800)
  )

  result <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure",
    include_lowest = FALSE
  )

  expect_true("highest_category" %in% names(result))
  expect_false("lowest_category" %in% names(result))

})

test_that("risk_distinct_category errors for missing columns", {

  x <- data.frame(
    unit_id = c("A", "B"),
    event_count = c(1, 2),
    exposure = c(100, 200)
  )

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "Missing required column"
  )

})