test_that("risk_crosstab_view returns a gt table", {

  skip_if_not_installed("gt")

  x <- data.frame(
    region = "North",
    Major = "P = 0.393\nn = 2 | 1.0/year",
    Total = "n = 2 | 1.0/year"
  )

  attr(x, "display") <- "detailed"
  attr(x, "poisson_k") <- 2L

  out <- risk_crosstab_view(
    x,
    rows = "region",
    title = "Risk Cross-Tab"
  )

  expect_s3_class(out, "gt_tbl")
})


test_that("risk_crosstab_view is silent during normal construction", {

  skip_if_not_installed("gt")

  x <- data.frame(
    region = "North",
    Major = "P = 0.393\nn = 2 | 1.0/year"
  )

  attr(x, "display") <- "detailed"
  attr(x, "poisson_k") <- 2L

  expect_silent(
    risk_crosstab_view(
      x,
      rows = "region"
    )
  )
})
