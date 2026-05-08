# 00001 — risk_calc_rate() --------------------------------------------------

test_that("risk_calc_rate calculates rates correctly", {

  x <- data.frame(
    event_count = c(5, 10, 0),
    exposure = c(100, 200, 0)
  )

  result <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "exposure",
    multiplier = 10000
  )

  expect_true("rate_per_10000" %in% names(result))
  expect_equal(result$rate_per_10000[1], 500)
  expect_equal(result$rate_per_10000[2], 500)
  expect_true(is.na(result$rate_per_10000[3]))

})

# 00002 — risk_calc_poisson_probability() ----------------------------------

test_that("risk_calc_poisson_probability calculates probability fields", {

  x <- data.frame(
    event_count = c(0, 1, 5),
    years = c(1, 1, 5)
  )

  result <- risk_calc_poisson_probability(
    data = x,
    count_col = "event_count",
    period_col = "years",
    output = "both"
  )

  expect_true("lambda" %in% names(result))
  expect_true("prob_event_ge_1" %in% names(result))
  expect_true("prob_event_ge_1_pct" %in% names(result))

  expect_equal(result$lambda[1], 0)
  expect_equal(result$lambda[2], 1)
  expect_equal(result$lambda[3], 1)

  expect_equal(result$prob_event_ge_1[1], 0)
  expect_equal(result$prob_event_ge_1_pct[1], 0)

})

# 00003 — risk_calc_smr() ---------------------------------------------------

test_that("risk_calc_smr returns expected SMR fields", {

  x <- data.frame(
    event_count = c(5, 10, 0),
    exposure = c(100, 200, 100)
  )

  result <- risk_calc_smr(
    data = x,
    observed_col = "event_count",
    denominator_col = "exposure"
  )

  expect_true("expected_count" %in% names(result))
  expect_true("smr" %in% names(result))
  expect_true("smr_lower" %in% names(result))
  expect_true("smr_upper" %in% names(result))
  expect_true("smr_ci_flag" %in% names(result))

  expect_true(all(result$expected_count >= 0, na.rm = TRUE))

})

# 00004 — risk_calc_location_quotient() ------------------------------------

test_that("risk_calc_location_quotient returns LQ fields", {

  x <- data.frame(
    event_count = c(5, 10, 0),
    exposure = c(100, 200, 100)
  )

  result <- risk_calc_location_quotient(
    data = x,
    observed_col = "event_count",
    denominator_col = "exposure"
  )

  expect_true("location_quotient" %in% names(result))
  expect_true("local_rate" %in% names(result))

  expect_true(all(result$local_rate >= 0, na.rm = TRUE))

})