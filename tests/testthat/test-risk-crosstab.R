# ==============================================================================
# 1. test-risk-crosstab.R
# Purpose:
#   Production tests for risk_crosstab().
# ==============================================================================


# ==============================================================================
# 2. TEST FIXTURES
# ==============================================================================

make_crosstab_events <- function() {
  data.frame(
    region = factor(
      c("North", "North", "North", "South", "South", NA),
      levels = c("North", "South", "West")
    ),
    consequence = factor(
      c("Minor", "Minor", "Major", "Minor", "Fatal", NA),
      levels = c("Minor", "Moderate", "Major", "Severe", "Fatal")
    )
  )
}


# ==============================================================================
# 3. BASIC RECORD COUNTS
# ==============================================================================

test_that("risk_crosstab counts records by cell", {
  out <- risk_crosstab(
    make_crosstab_events(),
    rows = "region",
    cols = "consequence"
  )

  target <- out[
    which(
      out$region == "North" &
        out$consequence == "Minor"
    ),
    ,
    drop = FALSE
  ]

  expect_equal(target$event_count, 2)
})


test_that("risk_crosstab retains zero-event combinations", {
  out <- risk_crosstab(
    make_crosstab_events(),
    rows = "region",
    cols = "consequence"
  )

  target <- out[
    which(
      out$region == "West" &
        out$consequence == "Fatal"
    ),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(target), 1L)
  expect_equal(target$event_count, 0)
})


# ==============================================================================
# 4. FACTOR LEVEL COMPLETION
# ==============================================================================

test_that("risk_crosstab retains factor category levels", {
  out <- risk_crosstab(
    make_crosstab_events(),
    rows = "region",
    cols = "consequence"
  )

  expect_true("West" %in% out$region)
  expect_true("Moderate" %in% out$consequence)
  expect_true("Severe" %in% out$consequence)
})


# ==============================================================================
# 5. MISSING CATEGORIES
# ==============================================================================

test_that("risk_crosstab retains missing categories by default", {
  out <- risk_crosstab(
    make_crosstab_events(),
    rows = "region",
    cols = "consequence"
  )

  expect_true(any(is.na(out$region)))
  expect_true(any(is.na(out$consequence)))
})


test_that("risk_crosstab can exclude missing categories", {
  out <- risk_crosstab(
    make_crosstab_events(),
    rows = "region",
    cols = "consequence",
    include_missing = FALSE
  )

  expect_false(any(is.na(out$region)))
  expect_false(any(is.na(out$consequence)))
})


# ==============================================================================
# 6. SUM MODE
# ==============================================================================

test_that("risk_crosstab sums pre-aggregated event counts", {
  x <- data.frame(
    region = c("North", "North", "South"),
    consequence = c("Minor", "Minor", "Minor"),
    event_count = c(2, 3, 4)
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence",
    value_col = "event_count",
    measure = "sum"
  )

  target <- out[
    which(
      out$region == "North" &
        out$consequence == "Minor"
    ),
    ,
    drop = FALSE
  ]

  expect_equal(target$event_count, 5)
})


# ==============================================================================
# 7. ANNUALISATION
# ==============================================================================

test_that("risk_crosstab annualises cell counts", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence",
    start_date = as.Date("2024-07-01"),
    end_date = as.Date("2026-06-30")
  )

  expect_false(is.na(out$period_years))
  expect_false(is.na(out$annualised_count))
  expect_equal(round(out$period_years, 2), 2)
  expect_equal(round(out$annualised_count, 1), 0.5)
})


test_that("risk_crosstab leaves annualised metrics NA without dates", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence"
  )

  expect_true(all(is.na(out$period_years)))
  expect_true(all(is.na(out$annualised_count)))
})


# ==============================================================================
# 8. EXPECTED COUNTS AND FIXED POISSON THRESHOLD
# ==============================================================================

test_that("risk_crosstab defaults to P(X >= 1)", {
  x <- data.frame(
    region = rep("North", 3),
    consequence = rep("Minor", 3)
  )

  expected <- data.frame(
    region = "North",
    consequence = "Minor",
    expected_count = 2
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence",
    expected_data = expected
  )

  expect_equal(out$expected_count, 2)
  expect_equal(out$poisson_k, 1L)

  expect_equal(
    out$poisson_probability,
    stats::ppois(
      q = 0,
      lambda = 2,
      lower.tail = FALSE
    )
  )
})


test_that("risk_crosstab supports a user-selected Poisson threshold", {
  x <- data.frame(
    region = rep("North", 3),
    consequence = rep("Minor", 3)
  )

  expected <- data.frame(
    region = "North",
    consequence = "Minor",
    expected_count = 2
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence",
    expected_data = expected,
    poisson_k = 2
  )

  expect_equal(out$poisson_k, 2L)

  expect_equal(
    out$poisson_probability,
    stats::ppois(
      q = 1,
      lambda = 2,
      lower.tail = FALSE
    )
  )
})


test_that("risk_crosstab Poisson threshold is independent of observed cell count", {
  x <- data.frame(
    region = factor(
      "North",
      levels = c("North", "South")
    ),
    consequence = factor(
      "Minor",
      levels = c("Minor", "Fatal")
    )
  )

  expected <- data.frame(
    region = "North",
    consequence = "Fatal",
    expected_count = 0.2
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence",
    expected_data = expected,
    poisson_k = 1
  )

  target <- out[
    which(
      out$region == "North" &
        out$consequence == "Fatal"
    ),
    ,
    drop = FALSE
  ]

  expect_equal(target$event_count, 0)
  expect_equal(target$poisson_k, 1L)

  expect_equal(
    target$poisson_probability,
    stats::ppois(
      q = 0,
      lambda = 0.2,
      lower.tail = FALSE
    )
  )
})


test_that("risk_crosstab does not manufacture expected counts", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence",
    poisson_k = 2
  )

  expect_true(all(is.na(out$expected_count)))
  expect_true(all(is.na(out$poisson_probability)))
  expect_true(all(out$poisson_k == 2L))
})


# ==============================================================================
# 9. SF SUPPORT
# ==============================================================================

test_that("risk_crosstab accepts sf and returns non-spatial output", {
  skip_if_not_installed("sf")

  x <- sf::st_as_sf(
    data.frame(
      region = c("North", "South"),
      consequence = c("Minor", "Major"),
      x = c(145, 146),
      y = c(-37, -38)
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence"
  )

  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "sf"))
})


# ==============================================================================
# 10. ANALYTICAL OUTPUT CONTRACT
# ==============================================================================

test_that("risk_crosstab returns the analytical paper trail", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  out <- risk_crosstab(
    x,
    rows = "region",
    cols = "consequence"
  )

  expect_identical(
    names(out),
    c(
      "region",
      "consequence",
      "event_count",
      "period_years",
      "annualised_count",
      "expected_count",
      "poisson_k",
      "poisson_probability"
    )
  )
})


# ==============================================================================
# 11. EXPECTED-COUNT VALIDATION
# ==============================================================================

test_that("risk_crosstab rejects duplicate expected-count keys", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  expected <- data.frame(
    region = c("North", "North"),
    consequence = c("Minor", "Minor"),
    expected_count = c(1, 2)
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      expected_data = expected
    ),
    "unique row and column keys"
  )
})


test_that("risk_crosstab rejects invalid expected counts", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  expected <- data.frame(
    region = "North",
    consequence = "Minor",
    expected_count = -1
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      expected_data = expected
    ),
    "non-negative finite values"
  )
})


# ==============================================================================
# 12. COMPLETE ROW BEHAVIOUR
# ==============================================================================

test_that("complete_rows TRUE retains zero-observation factor rows", {
  x <- data.frame(
    park = factor(
      c("Park A", "Park A", "Park B"),
      levels = c("Park A", "Park B", "Park C")
    ),
    consequence = factor(
      c("Minor", "Major", "Minor"),
      levels = c("Minor", "Major", "Fatal")
    )
  )

  out <- risk_crosstab(
    x,
    rows = "park",
    cols = "consequence",
    complete_rows = TRUE
  )

  expect_true("Park C" %in% out$park)

  park_c <- out[
    which(out$park == "Park C"),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(park_c), 3L)
  expect_true(all(park_c$event_count == 0))
})


test_that("complete_rows FALSE drops unobserved factor rows", {
  x <- data.frame(
    park = factor(
      c("Park A", "Park A", "Park B"),
      levels = c("Park A", "Park B", "Park C")
    ),
    consequence = factor(
      c("Minor", "Major", "Minor"),
      levels = c("Minor", "Major", "Fatal")
    )
  )

  out <- risk_crosstab(
    x,
    rows = "park",
    cols = "consequence",
    complete_rows = FALSE
  )

  expect_setequal(
    unique(out$park),
    c("Park A", "Park B")
  )

  expect_false("Park C" %in% out$park)
})


test_that("complete_rows FALSE retains complete column structure", {
  x <- data.frame(
    park = factor(
      c("Park A", "Park A", "Park B"),
      levels = c("Park A", "Park B", "Park C")
    ),
    consequence = factor(
      c("Minor", "Major", "Minor"),
      levels = c("Minor", "Major", "Fatal")
    )
  )

  out <- risk_crosstab(
    x,
    rows = "park",
    cols = "consequence",
    complete_rows = FALSE
  )

  expect_setequal(
    unique(out$consequence),
    c("Minor", "Major", "Fatal")
  )

  expect_equal(nrow(out), 6L)

  fatal <- out[
    which(out$consequence == "Fatal"),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(fatal), 2L)
  expect_true(all(fatal$event_count == 0))
})


# ==============================================================================
# 13. ARGUMENT VALIDATION
# ==============================================================================

test_that("complete_rows must be a single logical value", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      complete_rows = NA
    ),
    "complete_rows must be TRUE or FALSE"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      complete_rows = "no"
    ),
    "complete_rows must be TRUE or FALSE"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      complete_rows = c(TRUE, FALSE)
    ),
    "complete_rows must be TRUE or FALSE"
  )
})


test_that("risk_crosstab requires valid grouping columns", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "missing",
      cols = "consequence"
    ),
    "rows and cols must name columns present in data"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "missing"
    ),
    "rows and cols must name columns present in data"
  )
})


test_that("sum mode requires a numeric value column", {
  x <- data.frame(
    region = "North",
    consequence = "Minor",
    value = "three"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      measure = "sum"
    ),
    "value_col must name a column in data"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      value_col = "value",
      measure = "sum"
    ),
    "value_col must be numeric"
  )
})


test_that("start and end dates must be supplied together", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      start_date = as.Date("2024-07-01")
    ),
    "start_date and end_date must be supplied together"
  )
})


test_that("poisson_k must be a single whole number greater than or equal to one", {
  x <- data.frame(
    region = "North",
    consequence = "Minor"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      poisson_k = 0
    ),
    "greater than or equal to 1"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      poisson_k = 1.5
    ),
    "whole number"
  )

  expect_error(
    risk_crosstab(
      x,
      rows = "region",
      cols = "consequence",
      poisson_k = c(1, 2)
    ),
    "single whole number"
  )
})


# ==============================================================================
# 14. SILENT EXECUTION
# ==============================================================================

test_that("risk_crosstab is silent during normal execution", {
  x <- data.frame(
    region = c("North", "South"),
    consequence = c("Minor", "Major")
  )

  expect_silent(
    out <- risk_crosstab(
      x,
      rows = "region",
      cols = "consequence"
    )
  )

  expect_s3_class(out, "data.frame")
})
