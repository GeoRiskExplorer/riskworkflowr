# ============================================================================
# riskworkflowr
# TESTS — risk_calc_smr()
# ============================================================================


# ============================================================================
# 01. CORE CALCULATION
# ============================================================================

test_that("risk_calc_smr calculates expected counts and SMR correctly", {

  x <- data.frame(
    unit = c("A", "B", "C"),
    event_count = c(5, 10, 20),
    population = c(1000, 2000, 3000)
  )

  out <- risk_calc_smr(
    data = x,
    observed_col = "event_count",
    denominator_col = "population",
    global_rate_col = "global_rate",
    ci_method = "none"
  )

  expected_global_rate <-
    sum(x$event_count) /
    sum(x$population)

  expected_counts <-
    x$population * expected_global_rate

  expected_smr <-
    x$event_count / expected_counts

  expect_equal(
    unique(out$global_rate),
    expected_global_rate
  )

  expect_equal(
    out$expected_count,
    expected_counts
  )

  expect_equal(
    out$smr,
    expected_smr
  )
})


# ============================================================================
# 02. REFERENCE RATE IS DENOMINATOR-WEIGHTED
# ============================================================================

test_that("reference rate is calculated from dataset totals", {

  x <- data.frame(
    event_count = c(10, 10),
    population = c(100, 10000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    global_rate_col = "global_rate",
    ci_method = "none"
  )

  expected_rate <-
    sum(x$event_count) /
    sum(x$population)

  expect_equal(
    unique(out$global_rate),
    expected_rate
  )
})


# ============================================================================
# 03. SMR INTERPRETATION
# ============================================================================

test_that("SMR values represent observed relative to expected", {

  x <- data.frame(
    event_count = c(5, 10, 20),
    population = c(1000, 1000, 1000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none"
  )

  expect_lt(out$smr[1], 1)
  expect_lt(out$smr[2], 1)
  expect_gt(out$smr[3], 1)
})


# ============================================================================
# 04. ZERO OBSERVED EVENTS
# ============================================================================

test_that("zero observed events produce zero SMR when expected is positive", {

  x <- data.frame(
    event_count = c(0, 10),
    population = c(1000, 1000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population"
  )

  expect_equal(out$smr[1], 0)
  expect_equal(out$smr_lower[1], 0)

  expect_true(
    is.finite(out$smr_upper[1])
  )
})


# ============================================================================
# 05. EXACT POISSON CONFIDENCE INTERVALS
# ============================================================================

test_that("exact Poisson confidence intervals are calculated correctly", {

  x <- data.frame(
    event_count = c(0, 5, 20),
    population = c(1000, 1000, 1000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "exact",
    conf_level = 0.95
  )

  alpha <- 0.05

  expected_lower_observed <- c(
    0,
    stats::qchisq(alpha / 2, 2 * 5) / 2,
    stats::qchisq(alpha / 2, 2 * 20) / 2
  )

  expected_upper_observed <- c(
    stats::qchisq(
      1 - alpha / 2,
      2 * (0 + 1)
    ) / 2,
    stats::qchisq(
      1 - alpha / 2,
      2 * (5 + 1)
    ) / 2,
    stats::qchisq(
      1 - alpha / 2,
      2 * (20 + 1)
    ) / 2
  )

  expect_equal(
    out$smr_lower,
    expected_lower_observed /
      out$expected_count
  )

  expect_equal(
    out$smr_upper,
    expected_upper_observed /
      out$expected_count
  )
})


# ============================================================================
# 06. CI FLAG CLASSIFICATION
# ============================================================================

test_that("SMR confidence interval flags are classified correctly", {

  x <- data.frame(
    event_count = c(0, 5, 20),
    population = c(1000, 1000, 1000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "exact"
  )

  expect_equal(
    out$smr_ci_flag,
    c(
      "below_expected",
      "not_clearly_different",
      "above_expected"
    )
  )
})


# ============================================================================
# 07. CI METHOD NONE
# ============================================================================

test_that("ci_method none does not create confidence interval columns", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 1000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none"
  )

  expect_false(
    "smr_lower" %in% names(out)
  )

  expect_false(
    "smr_upper" %in% names(out)
  )

  expect_false(
    "smr_ci_flag" %in% names(out)
  )
})


# ============================================================================
# 08. OPTIONAL GLOBAL RATE COLUMN
# ============================================================================

test_that("global reference rate column is optional", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 2000)
  )

  without_rate <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none"
  )

  with_rate <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    global_rate_col = "reference_rate",
    ci_method = "none"
  )

  expect_false(
    "reference_rate" %in% names(without_rate)
  )

  expect_true(
    "reference_rate" %in% names(with_rate)
  )

  expect_length(
    unique(with_rate$reference_rate),
    1
  )
})


# ============================================================================
# 09. MISSING VALUES
# ============================================================================

test_that("missing values propagate to affected row-level outputs", {

  x <- data.frame(
    event_count = c(5, NA_real_, 10),
    population = c(1000, 1000, NA_real_)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population"
  )

  expect_false(
    is.na(out$smr[1])
  )

  expect_true(
    is.na(out$smr[2])
  )

  expect_true(
    is.na(out$smr[3])
  )

  expect_equal(
    out$smr_ci_flag[2],
    "missing"
  )

  expect_equal(
    out$smr_ci_flag[3],
    "missing"
  )
})


# ============================================================================
# 10. ZERO EXPECTED VALUE
# ============================================================================

test_that("zero expected counts use zero_expected_value", {

  x <- data.frame(
    event_count = c(0, 10),
    population = c(0, 1000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none"
  )

  expect_true(
    is.na(out$smr[1])
  )

  custom <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none",
    zero_expected_value = 0
  )

  expect_equal(
    custom$smr[1],
    0
  )
})


# ============================================================================
# 11. NEGATIVE OBSERVED COUNTS
# ============================================================================

test_that("negative observed counts are rejected", {

  x <- data.frame(
    event_count = c(5, -1),
    population = c(1000, 1000)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population"
    ),
    "greater than or equal to zero"
  )
})


# ============================================================================
# 12. NEGATIVE DENOMINATORS
# ============================================================================

test_that("negative denominators are rejected", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, -100)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population"
    ),
    "greater than or equal to zero"
  )
})


# ============================================================================
# 13. NON-NUMERIC OBSERVED COUNTS
# ============================================================================

test_that("non-numeric observed values are rejected", {

  x <- data.frame(
    event_count = c("5", "10"),
    population = c(1000, 1000)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population"
    ),
    "must be numeric"
  )
})


# ============================================================================
# 14. NON-NUMERIC DENOMINATORS
# ============================================================================

test_that("non-numeric denominators are rejected", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c("1000", "1000")
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population"
    ),
    "must be numeric"
  )
})


# ============================================================================
# 15. INFINITE VALUES
# ============================================================================

test_that("infinite observed and denominator values are rejected", {

  observed_inf <- data.frame(
    event_count = c(5, Inf),
    population = c(1000, 1000)
  )

  denominator_inf <- data.frame(
    event_count = c(5, 10),
    population = c(1000, Inf)
  )

  expect_error(
    risk_calc_smr(
      data = observed_inf,
      denominator_col = "population"
    ),
    "finite values or NA"
  )

  expect_error(
    risk_calc_smr(
      data = denominator_inf,
      denominator_col = "population"
    ),
    "finite values or NA"
  )
})


# ============================================================================
# 16. FRACTIONAL COUNTS WITH EXACT CI
# ============================================================================

test_that("fractional observed counts are rejected for exact Poisson CI", {

  x <- data.frame(
    event_count = c(1.5, 5),
    population = c(1000, 1000)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population",
      ci_method = "exact"
    ),
    "whole-number counts"
  )
})


# ============================================================================
# 17. FRACTIONAL COUNTS WITHOUT CI
# ============================================================================

test_that("fractional observed values are allowed when CI is disabled", {

  x <- data.frame(
    event_count = c(1.5, 5),
    population = c(1000, 1000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none"
  )

  expect_true(
    all(is.finite(out$smr))
  )
})


# ============================================================================
# 18. INVALID CONFIDENCE LEVEL
# ============================================================================

test_that("invalid confidence levels are rejected", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 1000)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population",
      conf_level = 0
    ),
    "between 0 and 1"
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population",
      conf_level = 1
    ),
    "between 0 and 1"
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population",
      conf_level = NA_real_
    ),
    "between 0 and 1"
  )
})


# ============================================================================
# 19. ZERO TOTAL DENOMINATOR
# ============================================================================

test_that("zero total denominator is rejected for non-empty data", {

  x <- data.frame(
    event_count = c(0, 0),
    population = c(0, 0)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population"
    ),
    "Total denominator must be greater than zero"
  )
})


# ============================================================================
# 20. OUTPUT COLUMN COLLISIONS
# ============================================================================

test_that("existing output columns are rejected", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 1000),
    smr = c(99, 99)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population"
    ),
    "already exists"
  )
})


# ============================================================================
# 21. CUSTOM OUTPUT COLUMN COLLISIONS
# ============================================================================

test_that("custom output column collisions are rejected", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 1000),
    custom_expected = c(1, 1)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population",
      expected_col = "custom_expected"
    ),
    "already exists"
  )
})


# ============================================================================
# 22. MISSING INPUT COLUMNS
# ============================================================================

test_that("missing input columns are rejected", {

  x <- data.frame(
    event_count = c(5, 10)
  )

  expect_error(
    risk_calc_smr(
      data = x,
      denominator_col = "population"
    ),
    "denominator_col.*not found"
  )

  y <- data.frame(
    population = c(1000, 1000)
  )

  expect_error(
    risk_calc_smr(
      data = y,
      observed_col = "event_count",
      denominator_col = "population"
    ),
    "observed_col.*not found"
  )
})


# ============================================================================
# 23. ZERO-ROW INPUT
# ============================================================================

test_that("zero-row input returns zero-row output cleanly", {

  x <- data.frame(
    event_count = numeric(0),
    population = numeric(0)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population"
  )

  expect_equal(
    nrow(out),
    0
  )

  expect_true(
    all(
      c(
        "expected_count",
        "smr",
        "smr_lower",
        "smr_upper",
        "smr_ci_flag"
      ) %in% names(out)
    )
  )
})


# ============================================================================
# 24. ORIGINAL INPUT VALUES ARE PRESERVED
# ============================================================================

test_that("existing input columns are preserved", {

  x <- data.frame(
    unit = c("A", "B"),
    event_count = c(5, 10),
    population = c(1000, 2000)
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none"
  )

  expect_equal(
    out$unit,
    x$unit
  )

  expect_equal(
    out$event_count,
    x$event_count
  )

  expect_equal(
    out$population,
    x$population
  )
})


# ============================================================================
# 25. PIPEABILITY
# ============================================================================

test_that("risk_calc_smr works in a base-pipe workflow", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 2000)
  )

  out <- x |>
    risk_calc_smr(
      denominator_col = "population",
      ci_method = "none"
    )

  expect_s3_class(
    out,
    "data.frame"
  )

  expect_equal(
    nrow(out),
    nrow(x)
  )
})


# ============================================================================
# 26. SF GEOMETRY PRESERVATION
# ============================================================================

test_that("sf geometry and CRS are preserved", {

  skip_if_not_installed("sf")

  x <- sf::st_as_sf(
    data.frame(
      unit = c("A", "B"),
      event_count = c(5, 10),
      population = c(1000, 2000),
      x = c(144.9, 145.0),
      y = c(-37.8, -37.9)
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  out <- risk_calc_smr(
    data = x,
    denominator_col = "population",
    ci_method = "none"
  )

  expect_s3_class(
    out,
    "sf"
  )

  expect_equal(
    sf::st_geometry(out),
    sf::st_geometry(x)
  )

  expect_equal(
    sf::st_crs(out),
    sf::st_crs(x)
  )
})