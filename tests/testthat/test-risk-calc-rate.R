# ============================================================================
# test-risk-calc-rate.R
# Purpose: Formal contract tests for risk_calc_rate()
# ============================================================================


# ============================================================================
# 01. BASIC CALCULATION
# ============================================================================

test_that("risk_calc_rate calculates rates correctly", {

  x <- data.frame(
    event_count = c(5, 10, 15),
    population = c(1000, 2000, 3000)
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_equal(
    out$rate_per_10000,
    c(50, 50, 50)
  )
})


# ============================================================================
# 02. CUSTOM MULTIPLIER
# ============================================================================

test_that("risk_calc_rate supports a custom multiplier", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 2000)
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population",
    multiplier = 100000
  )

  expect_equal(
    out$rate_per_10000,
    c(500, 500)
  )
})


# ============================================================================
# 03. CUSTOM OUTPUT COLUMN
# ============================================================================

test_that("risk_calc_rate supports a custom output column", {

  x <- data.frame(
    event_count = 5,
    population = 1000
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population",
    rate_col = "risk_rate"
  )

  expect_true("risk_rate" %in% names(out))
  expect_equal(out$risk_rate, 50)
})


# ============================================================================
# 04. ZERO COUNT
# ============================================================================

test_that("risk_calc_rate returns zero for zero counts", {

  x <- data.frame(
    event_count = 0,
    population = 1000
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_equal(out$rate_per_10000, 0)
})


# ============================================================================
# 05. ZERO DENOMINATOR
# ============================================================================

test_that("risk_calc_rate handles zero denominators", {

  x <- data.frame(
    event_count = 5,
    population = 0
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_true(is.na(out$rate_per_10000))
})


# ============================================================================
# 06. CUSTOM ZERO DENOMINATOR VALUE
# ============================================================================

test_that("risk_calc_rate supports a custom zero denominator value", {

  x <- data.frame(
    event_count = 5,
    population = 0
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population",
    zero_denominator_value = 0
  )

  expect_equal(out$rate_per_10000, 0)
})


# ============================================================================
# 07. MISSING DENOMINATOR
# ============================================================================

test_that("risk_calc_rate handles missing denominators", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, NA_real_)
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_equal(out$rate_per_10000[1], 50)
  expect_true(is.na(out$rate_per_10000[2]))
})


# ============================================================================
# 08. MISSING COUNT
# ============================================================================

test_that("risk_calc_rate propagates missing counts", {

  x <- data.frame(
    event_count = c(5, NA_real_),
    population = c(1000, 2000)
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_equal(out$rate_per_10000[1], 50)
  expect_true(is.na(out$rate_per_10000[2]))
})


# ============================================================================
# 09. NEGATIVE COUNT
# ============================================================================

test_that("risk_calc_rate rejects negative counts", {

  x <- data.frame(
    event_count = c(5, -1),
    population = c(1000, 1000)
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population"
    ),
    "cannot contain negative"
  )
})


# ============================================================================
# 10. NEGATIVE DENOMINATOR
# ============================================================================

test_that("risk_calc_rate rejects negative denominators", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, -2000)
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population"
    ),
    "cannot contain negative"
  )
})


# ============================================================================
# 11. NON-NUMERIC COUNT
# ============================================================================

test_that("risk_calc_rate rejects non-numeric counts", {

  x <- data.frame(
    event_count = c("5", "10"),
    population = c(1000, 2000)
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population"
    ),
    "must contain numeric"
  )
})


# ============================================================================
# 12. NON-NUMERIC DENOMINATOR
# ============================================================================

test_that("risk_calc_rate rejects non-numeric denominators", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c("1000", "2000")
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population"
    ),
    "must contain numeric"
  )
})


# ============================================================================
# 13. INFINITE COUNT
# ============================================================================

test_that("risk_calc_rate rejects infinite counts", {

  x <- data.frame(
    event_count = c(5, Inf),
    population = c(1000, 2000)
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population"
    ),
    "finite values"
  )
})


# ============================================================================
# 14. INFINITE DENOMINATOR
# ============================================================================

test_that("risk_calc_rate rejects infinite denominators", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, Inf)
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population"
    ),
    "finite values"
  )
})


# ============================================================================
# 15. INVALID MULTIPLIER
# ============================================================================

test_that("risk_calc_rate rejects invalid multipliers", {

  x <- data.frame(
    event_count = 5,
    population = 1000
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population",
      multiplier = 0
    ),
    "finite positive"
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population",
      multiplier = -100
    ),
    "finite positive"
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population",
      multiplier = Inf
    ),
    "finite positive"
  )
})


# ============================================================================
# 16. INVALID FALLBACK VALUE
# ============================================================================

test_that("risk_calc_rate validates zero_denominator_value", {

  x <- data.frame(
    event_count = 5,
    population = 1000
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population",
      zero_denominator_value = Inf
    ),
    "finite numeric value or NA"
  )
})


# ============================================================================
# 17. OUTPUT COLUMN COLLISION
# ============================================================================

test_that("risk_calc_rate rejects output column collisions", {

  x <- data.frame(
    event_count = 5,
    population = 1000,
    rate_per_10000 = 99
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "population"
    ),
    "already exists"
  )
})


# ============================================================================
# 18. MISSING INPUT COLUMNS
# ============================================================================

test_that("risk_calc_rate rejects missing input columns", {

  x <- data.frame(
    event_count = 5,
    population = 1000
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "missing_count",
      denominator_col = "population"
    ),
    "not found"
  )

  expect_error(
    risk_calc_rate(
      data = x,
      count_col = "event_count",
      denominator_col = "missing_denominator"
    ),
    "not found"
  )
})


# ============================================================================
# 19. ZERO-ROW INPUT
# ============================================================================

test_that("risk_calc_rate supports zero-row input", {

  x <- data.frame(
    event_count = numeric(),
    population = numeric()
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_equal(nrow(out), 0)
  expect_true("rate_per_10000" %in% names(out))
  expect_type(out$rate_per_10000, "double")
})


# ============================================================================
# 20. INPUT PRESERVATION
# ============================================================================

test_that("risk_calc_rate preserves existing columns and values", {

  x <- data.frame(
    unit = c("A", "B"),
    event_count = c(5, 10),
    population = c(1000, 2000)
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_equal(out$unit, x$unit)
  expect_equal(out$event_count, x$event_count)
  expect_equal(out$population, x$population)
})


# ============================================================================
# 21. PIPEABILITY
# ============================================================================

test_that("risk_calc_rate works in a base pipe", {

  x <- data.frame(
    event_count = c(5, 10),
    population = c(1000, 2000)
  )

  out <- x |>
    risk_calc_rate(
      count_col = "event_count",
      denominator_col = "population"
    )

  expect_equal(
    out$rate_per_10000,
    c(50, 50)
  )
})


# ============================================================================
# 22. SF PRESERVATION
# ============================================================================

test_that("risk_calc_rate preserves sf geometry and CRS", {

  skip_if_not_installed("sf")

  x <- sf::st_sf(
    unit = c("A", "B"),
    event_count = c(5, 10),
    population = c(1000, 2000),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 4326
    )
  )

  out <- risk_calc_rate(
    data = x,
    count_col = "event_count",
    denominator_col = "population"
  )

  expect_s3_class(out, "sf")
  expect_equal(
    sf::st_crs(out),
    sf::st_crs(x)
  )
  expect_equal(
    sf::st_geometry(out),
    sf::st_geometry(x)
  )
  expect_equal(
    out$rate_per_10000,
    c(50, 50)
  )
})