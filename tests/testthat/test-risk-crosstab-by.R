# ==============================================================================
# 1. test-risk-crosstab-by.R
# Purpose:
#   Production tests for risk_crosstab_by().
# ==============================================================================


# ==============================================================================
# 2. GROUPED CROSS-TABS
# ==============================================================================

test_that("risk_crosstab_by repeats cross-tabs within groups", {
  x <- data.frame(
    region = c("North", "North", "South", "South", "South"),
    activity = c("Walking", "Walking", "Walking", "Fishing", "Fishing"),
    consequence = c("Minor", "Major", "Minor", "Minor", "Minor")
  )

  out <- risk_crosstab_by(
    x,
    by = "region",
    rows = "activity",
    cols = "consequence",
    complete_rows = FALSE
  )

  expect_setequal(
    unique(out$region),
    c("North", "South")
  )

  target <- out[
    which(
      out$region == "South" &
        out$activity == "Fishing" &
        out$consequence == "Minor"
    ),
    ,
    drop = FALSE
  ]

  expect_equal(target$event_count, 2)
})


test_that("risk_crosstab_by retains complete factor column structure within groups", {
  x <- data.frame(
    region = c("North", "South"),
    activity = c("Walking", "Fishing"),
    consequence = factor(
      c("Minor", "Major"),
      levels = c("Minor", "Moderate", "Major", "Severe", "Fatal")
    )
  )

  out <- risk_crosstab_by(
    x,
    by = "region",
    rows = "activity",
    cols = "consequence",
    complete_rows = FALSE
  )

  expect_equal(
    length(
      unique(
        out$consequence[
          which(out$region == "North")
        ]
      )
    ),
    5L
  )

  expect_true(
    "Fatal" %in%
      out$consequence[
        which(out$region == "North")
      ]
  )
})


test_that("risk_crosstab_by supports multiple grouping variables", {
  x <- data.frame(
    year = c(2025, 2025, 2026, 2026),
    region = c("North", "South", "North", "South"),
    activity = "Walking",
    consequence = "Minor"
  )

  out <- risk_crosstab_by(
    x,
    by = c("year", "region"),
    rows = "activity",
    cols = "consequence"
  )

  expect_equal(
    nrow(
      unique(
        out[c("year", "region")]
      )
    ),
    4L
  )
})


# ==============================================================================
# 3. EXPECTED COUNTS AND POISSON THRESHOLD
# ==============================================================================

test_that("risk_crosstab_by subsets expected data by grouping variables", {
  x <- data.frame(
    region = c("North", "South"),
    activity = c("Walking", "Walking"),
    consequence = c("Minor", "Minor")
  )

  expected <- data.frame(
    region = c("North", "South"),
    activity = c("Walking", "Walking"),
    consequence = c("Minor", "Minor"),
    expected_count = c(0.5, 2)
  )

  out <- risk_crosstab_by(
    x,
    by = "region",
    rows = "activity",
    cols = "consequence",
    expected_data = expected
  )

  north <- out[
    which(out$region == "North"),
    ,
    drop = FALSE
  ]

  south <- out[
    which(out$region == "South"),
    ,
    drop = FALSE
  ]

  expect_equal(north$expected_count, 0.5)
  expect_equal(south$expected_count, 2)
})


test_that("risk_crosstab_by passes poisson_k through every group", {
  x <- data.frame(
    region = c("North", "South"),
    activity = c("Walking", "Walking"),
    consequence = c("Minor", "Minor")
  )

  expected <- data.frame(
    region = c("North", "South"),
    activity = c("Walking", "Walking"),
    consequence = c("Minor", "Minor"),
    expected_count = c(0.5, 2)
  )

  out <- risk_crosstab_by(
    x,
    by = "region",
    rows = "activity",
    cols = "consequence",
    expected_data = expected,
    poisson_k = 2
  )

  expect_true(all(out$poisson_k == 2L))

  north <- out[
    which(out$region == "North"),
    ,
    drop = FALSE
  ]

  south <- out[
    which(out$region == "South"),
    ,
    drop = FALSE
  ]

  expect_equal(
    north$poisson_probability,
    stats::ppois(
      q = 1,
      lambda = 0.5,
      lower.tail = FALSE
    )
  )

  expect_equal(
    south$poisson_probability,
    stats::ppois(
      q = 1,
      lambda = 2,
      lower.tail = FALSE
    )
  )
})


# ==============================================================================
# 4. SF SUPPORT
# ==============================================================================

test_that("risk_crosstab_by accepts sf and returns non-spatial output", {
  skip_if_not_installed("sf")

  x <- sf::st_as_sf(
    data.frame(
      region = c("North", "South"),
      activity = c("Walking", "Fishing"),
      consequence = c("Minor", "Major"),
      x = c(145, 146),
      y = c(-37, -38)
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  out <- risk_crosstab_by(
    x,
    by = "region",
    rows = "activity",
    cols = "consequence"
  )

  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "sf"))
})


# ==============================================================================
# 5. VALIDATION
# ==============================================================================

test_that("risk_crosstab_by validates grouping columns", {
  x <- data.frame(
    region = "North",
    activity = "Walking",
    consequence = "Minor"
  )

  expect_error(
    risk_crosstab_by(
      x,
      by = "missing",
      rows = "activity",
      cols = "consequence"
    ),
    "by, rows, and cols must name columns present in data"
  )
})


test_that("risk_crosstab_by rejects overlap between by and cross-tab dimensions", {
  x <- data.frame(
    region = "North",
    activity = "Walking",
    consequence = "Minor"
  )

  expect_error(
    risk_crosstab_by(
      x,
      by = "activity",
      rows = "activity",
      cols = "consequence"
    ),
    "by columns must be different from rows and cols"
  )

  expect_error(
    risk_crosstab_by(
      x,
      by = "consequence",
      rows = "activity",
      cols = "consequence"
    ),
    "by columns must be different from rows and cols"
  )
})


# ==============================================================================
# 6. SILENT EXECUTION
# ==============================================================================

test_that("risk_crosstab_by is silent during normal execution", {
  x <- data.frame(
    region = c("North", "South"),
    activity = c("Walking", "Fishing"),
    consequence = c("Minor", "Major")
  )

  expect_silent(
    out <- risk_crosstab_by(
      x,
      by = "region",
      rows = "activity",
      cols = "consequence"
    )
  )

  expect_s3_class(out, "data.frame")
})
