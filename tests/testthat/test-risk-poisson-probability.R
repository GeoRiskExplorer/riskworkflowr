# ============================================================================
# risk_poisson_probability()
# ============================================================================


testthat::test_that("calculates P(X >= 1) correctly", {

  x <- data.frame(
    lambda = c(0, 0.5, 1, 2)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda"
  )

  expected <- c(
    0,
    1 - exp(-0.5),
    1 - exp(-1),
    1 - exp(-2)
  )

  testthat::expect_equal(
    out$poisson_probability,
    expected,
    tolerance = 1e-12
  )
})


testthat::test_that("calculates P(X >= k) for k greater than 1", {

  x <- data.frame(
    lambda = c(0.5, 1, 2, 5)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda",
    k = 2
  )

  expected <- stats::ppois(
    q = 1,
    lambda = x$lambda,
    lower.tail = FALSE
  )

  testthat::expect_equal(
    out$poisson_probability,
    expected,
    tolerance = 1e-12
  )
})


testthat::test_that("supports larger event thresholds", {

  x <- data.frame(
    lambda = c(1, 2, 5)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda",
    k = 5
  )

  expected <- stats::ppois(
    q = 4,
    lambda = x$lambda,
    lower.tail = FALSE
  )

  testthat::expect_equal(
    out$poisson_probability,
    expected,
    tolerance = 1e-12
  )
})


testthat::test_that("zero lambda gives zero probability for k >= 1", {

  out <- risk_poisson_probability(
    data.frame(lambda = 0),
    lambda_col = "lambda",
    k = 4
  )

  testthat::expect_equal(
    out$poisson_probability,
    0
  )
})


testthat::test_that("missing lambda propagates to missing probability", {

  x <- data.frame(
    lambda = c(0.5, NA_real_, 2)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda"
  )

  testthat::expect_false(is.na(out$poisson_probability[1]))
  testthat::expect_true(is.na(out$poisson_probability[2]))
  testthat::expect_false(is.na(out$poisson_probability[3]))
})


testthat::test_that("probabilities remain bounded between zero and one", {

  x <- data.frame(
    lambda = c(0, 0.1, 1, 10, 100, 1000)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda",
    k = 3
  )

  testthat::expect_true(
    all(out$poisson_probability >= 0)
  )

  testthat::expect_true(
    all(out$poisson_probability <= 1)
  )
})


testthat::test_that("probability increases as lambda increases", {

  x <- data.frame(
    lambda = c(0, 0.1, 0.5, 1, 2, 5, 10)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda",
    k = 2
  )

  testthat::expect_true(
    all(diff(out$poisson_probability) >= 0)
  )
})


testthat::test_that("custom output column is supported", {

  out <- risk_poisson_probability(
    data.frame(lambda = c(0.5, 1)),
    lambda_col = "lambda",
    k = 2,
    probability_col = "prob_ge_2"
  )

  testthat::expect_true(
    "prob_ge_2" %in% names(out)
  )

  testthat::expect_false(
    "poisson_probability" %in% names(out)
  )
})


testthat::test_that("input columns and values are preserved", {

  x <- data.frame(
    id = 1:3,
    label = c("A", "B", "C"),
    lambda = c(0.5, 1, 2)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda",
    k = 2
  )

  testthat::expect_identical(out$id, x$id)
  testthat::expect_identical(out$label, x$label)
  testthat::expect_identical(out$lambda, x$lambda)
})


testthat::test_that("zero-row data are supported", {

  x <- data.frame(
    lambda = numeric(0)
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda"
  )

  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_true(
    "poisson_probability" %in% names(out)
  )
})


testthat::test_that("function is pipeable", {

  out <- data.frame(
    lambda = c(0.5, 1, 2)
  ) |>
    risk_poisson_probability(
      lambda_col = "lambda",
      k = 2
    )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(
    "poisson_probability" %in% names(out)
  )
})


testthat::test_that("sf geometry and CRS are preserved", {

  testthat::skip_if_not_installed("sf")

  x <- sf::st_as_sf(
    data.frame(
      id = 1:3,
      lambda = c(0.5, 1, 2),
      x = c(144.9, 145.0, 145.1),
      y = c(-37.8, -37.9, -38.0)
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  out <- risk_poisson_probability(
    x,
    lambda_col = "lambda",
    k = 2
  )

  testthat::expect_s3_class(out, "sf")

  testthat::expect_identical(
    sf::st_geometry(out),
    sf::st_geometry(x)
  )

  testthat::expect_identical(
    sf::st_crs(out),
    sf::st_crs(x)
  )
})


testthat::test_that("missing lambda column is rejected", {

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(x = 1),
      lambda_col = "lambda"
    ),
    "`lambda_col` not found"
  )
})


testthat::test_that("non-numeric lambda is rejected", {

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(lambda = "1"),
      lambda_col = "lambda"
    ),
    "`lambda_col` must be numeric"
  )
})


testthat::test_that("negative lambda is rejected", {

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(lambda = c(1, -0.1)),
      lambda_col = "lambda"
    ),
    "greater than or equal to zero"
  )
})


testthat::test_that("infinite lambda is rejected", {

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(lambda = Inf),
      lambda_col = "lambda"
    ),
    "finite values or NA"
  )
})


testthat::test_that("k must be a whole number >= 1", {

  invalid_k <- list(
    0,
    -1,
    1.5,
    NA_real_,
    Inf,
    c(1, 2),
    "2"
  )

  for (value in invalid_k) {

    testthat::expect_error(
      risk_poisson_probability(
        data.frame(lambda = 1),
        lambda_col = "lambda",
        k = value
      ),
      "`k` must be a single whole number"
    )
  }
})


testthat::test_that("invalid column names are rejected", {

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(lambda = 1),
      lambda_col = ""
    ),
    "single non-empty column name"
  )

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(lambda = 1),
      lambda_col = NA_character_
    ),
    "single non-empty column name"
  )

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(lambda = 1),
      lambda_col = "lambda",
      probability_col = ""
    ),
    "single non-empty column name"
  )
})


testthat::test_that("existing output column is not silently overwritten", {

  testthat::expect_error(
    risk_poisson_probability(
      data.frame(
        lambda = 1,
        poisson_probability = 0.2
      ),
      lambda_col = "lambda"
    ),
    "`probability_col` already exists"
  )
})


testthat::test_that("default k equals explicit k = 1", {

  x <- data.frame(
    lambda = c(0.5, 1, 2)
  )

  default <- risk_poisson_probability(
    x,
    lambda_col = "lambda"
  )

  explicit <- risk_poisson_probability(
    x,
    lambda_col = "lambda",
    k = 1
  )

  testthat::expect_identical(
    default$poisson_probability,
    explicit$poisson_probability
  )
})