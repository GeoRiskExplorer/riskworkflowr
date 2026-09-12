# 00001 — fixed observation period ------------------------------------------

test_that("risk_annualise annualises a fixed observation period", {

  x <- data.frame(
    event_count = c(0, 10, 20)
  )

  result <- risk_annualise(
    x,
    period_value = 2
  )

  expect_equal(
    result$annualised_count,
    c(0, 5, 10)
  )

  expect_equal(
    nrow(result),
    nrow(x)
  )
})


# 00002 — row-specific observation periods ---------------------------------

test_that("risk_annualise supports row-specific periods", {

  x <- data.frame(
    event_count = c(10, 10, 10),
    years = c(1, 2, 5)
  )

  result <- risk_annualise(
    x,
    period_col = "years",
    period_years_col = "period_years"
  )

  expect_equal(
    result$annualised_count,
    c(10, 5, 2)
  )

  expect_equal(
    result$period_years,
    c(1, 2, 5)
  )
})


# 00003 — fixed date period -------------------------------------------------

test_that("risk_annualise supports fixed Date windows", {

  x <- data.frame(
    event_count = 10
  )

  result <- risk_annualise(
    x,
    start_date = as.Date("2025-01-01"),
    end_date = as.Date("2025-12-31"),
    period_years_col = "years"
  )

  expect_equal(
    result$years,
    365 / 365.2425
  )

  expect_equal(
    result$annualised_count,
    10 / (365 / 365.2425)
  )
})


# 00004 — inclusive date semantics -----------------------------------------

test_that("date windows are inclusive", {

  x <- data.frame(
    event_count = 1
  )

  result <- risk_annualise(
    x,
    start_date = as.Date("2026-06-01"),
    end_date = as.Date("2026-06-01"),
    period_years_col = "years"
  )

  expect_equal(
    result$years,
    1 / 365.2425
  )
})


# 00005 — leap years --------------------------------------------------------

test_that("risk_annualise handles leap years", {

  x <- data.frame(
    event_count = 10
  )

  result <- risk_annualise(
    x,
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    period_years_col = "years"
  )

  expect_equal(
    result$years,
    366 / 365.2425
  )
})


# 00006 — date columns ------------------------------------------------------

test_that("risk_annualise supports row-specific Date columns", {

  x <- data.frame(
    event_count = c(10, 20),
    start = as.Date(
      c(
        "2025-01-01",
        "2024-01-01"
      )
    ),
    end = as.Date(
      c(
        "2025-12-31",
        "2024-12-31"
      )
    )
  )

  result <- risk_annualise(
    x,
    start_col = "start",
    end_col = "end",
    period_years_col = "years"
  )

  expect_equal(
    result$years,
    c(365, 366) / 365.2425
  )
})


# 00007 — zero and missing event counts ------------------------------------

test_that("zero and missing counts are preserved appropriately", {

  x <- data.frame(
    event_count = c(0, NA_real_, 10)
  )

  result <- risk_annualise(
    x,
    period_value = 2
  )

  expect_equal(
    result$annualised_count[1],
    0
  )

  expect_true(
    is.na(result$annualised_count[2])
  )

  expect_equal(
    result$annualised_count[3],
    5
  )
})


# 00008 — missing observation period ---------------------------------------

test_that("missing row-specific periods produce missing annualised values", {

  x <- data.frame(
    event_count = c(10, 10),
    years = c(2, NA_real_)
  )

  result <- risk_annualise(
    x,
    period_col = "years"
  )

  expect_equal(
    result$annualised_count[1],
    5
  )

  expect_true(
    is.na(result$annualised_count[2])
  )
})


# 00009 — pipeability -------------------------------------------------------

test_that("risk_annualise pipes as a data transformer", {

  result <- data.frame(
    unit = c("A", "B"),
    event_count = c(4, 10)
  ) |>
    risk_annualise(
      period_value = 2,
      annualised_col = "lambda_annual"
    )

  expect_equal(
    result$lambda_annual,
    c(2, 5)
  )
})


# 00010 — sf preservation ---------------------------------------------------

test_that("risk_annualise preserves sf geometry and CRS", {

  skip_if_not_installed("sf")

  x <- sf::st_as_sf(
    data.frame(
      event_count = c(2, 4),
      x = c(145, 145.1),
      y = c(-37.8, -37.9)
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  original_geometry <- sf::st_geometry(x)
  original_crs <- sf::st_crs(x)

  result <- risk_annualise(
    x,
    period_value = 2
  )

  expect_s3_class(
    result,
    "sf"
  )

  expect_identical(
    sf::st_crs(result),
    original_crs
  )

  expect_identical(
    sf::st_geometry(result),
    original_geometry
  )

  expect_equal(
    result$annualised_count,
    c(1, 2)
  )
})


# 00011 — zero-row data -----------------------------------------------------

test_that("risk_annualise handles zero-row data frames", {

  x <- data.frame(
    event_count = numeric()
  )

  result <- risk_annualise(
    x,
    period_value = 2
  )

  expect_equal(
    nrow(result),
    0
  )

  expect_true(
    "annualised_count" %in% names(result)
  )
})


# 00012 — invalid counts ----------------------------------------------------

test_that("invalid event counts are rejected", {

  expect_error(
    risk_annualise(
      data.frame(event_count = c(1, -1)),
      period_value = 1
    ),
    "negative"
  )

  expect_error(
    risk_annualise(
      data.frame(event_count = c(1, Inf)),
      period_value = 1
    ),
    "infinite"
  )

  expect_error(
    risk_annualise(
      data.frame(event_count = "5"),
      period_value = 1
    ),
    "numeric"
  )
})


# 00013 — invalid periods ---------------------------------------------------

test_that("invalid observation periods are rejected", {

  expect_error(
    risk_annualise(
      data.frame(event_count = 1),
      period_value = 0
    )
  )

  expect_error(
    risk_annualise(
      data.frame(event_count = 1),
      period_value = -1
    )
  )

  expect_error(
    risk_annualise(
      data.frame(
        event_count = 1,
        years = Inf
      ),
      period_col = "years"
    )
  )
})


# 00014 — conflicting period specifications --------------------------------

test_that("exactly one period specification is required", {

  x <- data.frame(
    event_count = 1
  )

  expect_error(
    risk_annualise(x)
  )

  expect_error(
    risk_annualise(
      x,
      period_value = 2,
      start_date = as.Date("2025-01-01"),
      end_date = as.Date("2025-12-31")
    )
  )
})


# 00015 — incomplete date specifications -----------------------------------

test_that("date specifications require both boundaries", {

  x <- data.frame(
    event_count = 1
  )

  expect_error(
    risk_annualise(
      x,
      start_date = as.Date("2025-01-01")
    )
  )

  expect_error(
    risk_annualise(
      x,
      start_col = "start"
    )
  )
})


# 00016 — reversed date ranges ---------------------------------------------

test_that("reversed date windows are rejected", {

  x <- data.frame(
    event_count = 1
  )

  expect_error(
    risk_annualise(
      x,
      start_date = as.Date("2026-12-31"),
      end_date = as.Date("2026-01-01")
    )
  )
})


# 00017 — output collision -------------------------------------------------

test_that("existing output columns are protected", {

  x <- data.frame(
    event_count = 5,
    annualised_count = 123
  )

  expect_error(
    risk_annualise(
      x,
      period_value = 1
    ),
    "already exists"
  )
})


# 00018 — invalid column names ---------------------------------------------

test_that("invalid column-name arguments are rejected", {

  x <- data.frame(
    event_count = 1
  )

  expect_error(
    risk_annualise(
      x,
      period_value = 1,
      annualised_col = ""
    )
  )

  expect_error(
    risk_annualise(
      x,
      period_value = 1,
      annualised_col = NA_character_
    )
  )
})


# 00019 — custom year length ------------------------------------------------

test_that("custom year lengths are respected", {

  x <- data.frame(
    event_count = 10
  )

  result <- risk_annualise(
    x,
    start_date = as.Date("2025-01-01"),
    end_date = as.Date("2025-12-31"),
    year_length = 365,
    period_years_col = "years"
  )

  expect_equal(
    result$years,
    1
  )

  expect_equal(
    result$annualised_count,
    10
  )
})


# 00020 — original data are not modified ----------------------------------

test_that("risk_annualise preserves original fields and values", {

  x <- data.frame(
    id = c("A", "B"),
    event_count = c(4, 8),
    category = c("one", "two")
  )

  result <- risk_annualise(
    x,
    period_value = 2
  )

  expect_identical(
    result$id,
    x$id
  )

  expect_identical(
    result$event_count,
    x$event_count
  )

  expect_identical(
    result$category,
    x$category
  )
})