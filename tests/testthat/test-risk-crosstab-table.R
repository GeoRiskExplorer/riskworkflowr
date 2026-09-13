# ==============================================================================
# 1. test-risk-crosstab-table.R
# Purpose:
#   Production tests for risk_crosstab_table().
# ==============================================================================


# ==============================================================================
# 2. FIXTURE
# ==============================================================================

make_crosstab_table_analysis <- function() {
  data.frame(
    region = c("North", "North", "South", NA),
    consequence = c("Minor", "Fatal", "Minor", NA),
    event_count = c(3, 0, 2, 1),
    period_years = rep(2, 4),
    annualised_count = c(1.5, 0, 1, 0.5),
    expected_count = c(2, 0.2, NA, NA),
    poisson_k = rep(1L, 4),
    poisson_probability = c(
      stats::ppois(0, 2, lower.tail = FALSE),
      stats::ppois(0, 0.2, lower.tail = FALSE),
      NA,
      NA
    )
  )
}


# ==============================================================================
# 3. BASIC TABLE
# ==============================================================================

test_that("risk_crosstab_table creates wide reporting output", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence"
  )

  expect_true(
    all(c("region", "Minor", "Fatal", "Unassigned") %in% names(out))
  )
})


# ==============================================================================
# 4. DETAILED DISPLAY
# ==============================================================================

test_that("risk_crosstab_table labels Poisson probability", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence",
    display = "detailed"
  )

  north <- out[out$region == "North", , drop = FALSE]

  expect_equal(
    north$Minor,
    paste0(
      "P = ",
      formatC(
        stats::ppois(0, 2, lower.tail = FALSE),
        format = "f",
        digits = 3
      ),
      "\nn = 3 | 1.5/year"
    )
  )
})


test_that("risk_crosstab_table retains count context without Poisson", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence"
  )

  south <- out[out$region == "South", , drop = FALSE]

  expect_equal(south$Minor, "n = 2 | 1.0/year")
})


test_that("risk_crosstab_table displays zero-event cells clearly", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence"
  )

  north <- out[out$region == "North", , drop = FALSE]

  expect_equal(north$Fatal, "0 events")
})


# ==============================================================================
# 5. COUNT DISPLAY
# ==============================================================================

test_that("risk_crosstab_table supports raw count display", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence",
    display = "count"
  )

  north <- out[out$region == "North", , drop = FALSE]

  expect_equal(north$Minor, "3")
  expect_equal(north$Fatal, "0")
})


# ==============================================================================
# 6. MISSING LABELS
# ==============================================================================

test_that("risk_crosstab_table displays missing categories as Unassigned", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence"
  )

  expect_true("Unassigned" %in% out$region)
  expect_true("Unassigned" %in% names(out))
})


test_that("risk_crosstab_table allows custom missing label", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence",
    missing_label = "Not assigned"
  )

  expect_true("Not assigned" %in% out$region)
  expect_true("Not assigned" %in% names(out))
})


# ==============================================================================
# 7. PRIVACY SUPPRESSION
# ==============================================================================

test_that("risk_crosstab_table does not suppress by default", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence"
  )

  north <- out[out$region == "North", , drop = FALSE]

  expect_match(north$Minor, "n = 3", fixed = TRUE)
})


test_that("risk_crosstab_table can suppress low positive counts", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence",
    suppress_below = 5
  )

  north <- out[out$region == "North", , drop = FALSE]

  expect_equal(north$Minor, "<5")
  expect_equal(north$Fatal, "0 events")
})


test_that("row totals are omitted when suppression is active", {
  x <- data.frame(
    region = c("North", "North"),
    consequence = c("Minor", "Major"),
    event_count = c(10, 3),
    period_years = c(2, 2),
    annualised_count = c(5, 1.5),
    expected_count = c(NA, NA),
    poisson_k = c(1L, 1L),
    poisson_probability = c(NA, NA)
  )

  expect_warning(
    out <- risk_crosstab_table(
      x,
      rows = "region",
      cols = "consequence",
      suppress_below = 5,
      row_total = TRUE
    ),
    "Row totals are omitted when suppression is active"
  )

  expect_false("Total" %in% names(out))
  expect_equal(out$Major, "<5")
})


# ==============================================================================
# 8. ROW TOTALS
# ==============================================================================

test_that("risk_crosstab_table can add detailed row totals", {
  x <- data.frame(
    region = c("North", "North", "South", "South"),
    consequence = c("Minor", "Major", "Minor", "Major"),
    event_count = c(3, 2, 4, 1),
    period_years = rep(2, 4),
    annualised_count = c(1.5, 1, 2, 0.5),
    expected_count = NA_real_,
    poisson_k = 1L,
    poisson_probability = NA_real_
  )

  out <- risk_crosstab_table(
    x,
    rows = "region",
    cols = "consequence",
    display = "detailed",
    row_total = TRUE
  )

  expect_true("Total" %in% names(out))

  north <- out[out$region == "North", , drop = FALSE]

  expect_equal(north$Total, "n = 5 | 2.5/year")
})


test_that("risk_crosstab_table can add count row totals", {
  x <- make_crosstab_table_analysis()

  out <- risk_crosstab_table(
    x,
    rows = "region",
    cols = "consequence",
    display = "count",
    row_total = TRUE
  )

  north <- out[out$region == "North", , drop = FALSE]

  expect_equal(north$Total, "3")
})


test_that("row totals are not added by default", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence"
  )

  expect_false("Total" %in% names(out))
})


# ==============================================================================
# 9. REPORTING METADATA
# ==============================================================================

test_that("risk_crosstab_table carries Poisson threshold metadata", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence",
    display = "detailed"
  )

  expect_equal(attr(out, "poisson_k"), 1L)
  expect_equal(attr(out, "display"), "detailed")
})


test_that("risk_crosstab_table carries alternative Poisson threshold metadata", {
  x <- make_crosstab_table_analysis()
  x$poisson_k <- 2L

  out <- risk_crosstab_table(
    x,
    rows = "region",
    cols = "consequence",
    display = "detailed"
  )

  expect_equal(attr(out, "poisson_k"), 2L)
})


# ==============================================================================
# 10. INPUT / OUTPUT CONTRACT
# ==============================================================================

test_that("risk_crosstab_table does not alter analytical input", {
  x <- make_crosstab_table_analysis()
  original <- x

  invisible(
    risk_crosstab_table(
      x,
      rows = "region",
      cols = "consequence",
      suppress_below = 5
    )
  )

  expect_identical(x, original)
})


test_that("risk_crosstab_table returns an ordinary data frame", {
  out <- risk_crosstab_table(
    make_crosstab_table_analysis(),
    rows = "region",
    cols = "consequence"
  )

  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "sf"))
})


# ==============================================================================
# 11. VALIDATION
# ==============================================================================

test_that("risk_crosstab_table validates required analytical columns", {
  x <- make_crosstab_table_analysis()
  x$poisson_probability <- NULL

  expect_error(
    risk_crosstab_table(
      x,
      rows = "region",
      cols = "consequence"
    ),
    "required analytical cross-tab columns"
  )
})


test_that("risk_crosstab_table validates suppression threshold", {
  expect_error(
    risk_crosstab_table(
      make_crosstab_table_analysis(),
      rows = "region",
      cols = "consequence",
      suppress_below = 0
    ),
    "suppress_below"
  )
})


# ==============================================================================
# 12. SILENT EXECUTION
# ==============================================================================

test_that("risk_crosstab_table is silent during normal execution", {
  expect_silent(
    out <- risk_crosstab_table(
      make_crosstab_table_analysis(),
      rows = "region",
      cols = "consequence",
      row_total = TRUE
    )
  )

  expect_s3_class(out, "data.frame")
})
