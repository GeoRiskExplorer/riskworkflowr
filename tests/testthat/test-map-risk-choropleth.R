# ============================================================================
# test-map-risk-choropleth.R
# ============================================================================


# ============================================================================
# 01. FIXTURE
# ============================================================================

make_map_sf <- function() {

  sf::st_sf(
    unit_id = LETTERS[1:5],
    event_count = c(
      0,
      2,
      5,
      10,
      NA
    ),
    rate = c(
      0,
      1.2,
      3.5,
      8.1,
      NA
    ),
    probability = c(
      0,
      0.1,
      0.4,
      0.9,
      NA
    ),
    smr = c(
      0,
      0.5,
      1,
      1.5,
      2.5
    ),
    category = c(
      "Falls",
      "Water",
      "Falls",
      "Wildlife",
      NA
    ),
    geometry = sf::st_sfc(
      lapply(
        0:4,
        function(i) {
          sf::st_polygon(
            list(
              rbind(
                c(i * 10, 0),
                c(i * 10 + 8, 0),
                c(i * 10 + 8, 8),
                c(i * 10, 8),
                c(i * 10, 0)
              )
            )
          )
        }
      ),
      crs = 3857
    )
  )
}


# ============================================================================
# 02. DEFAULT COUNT MAP
# ============================================================================

test_that("map_risk_choropleth returns ggplot for count data", {

  p <- map_risk_choropleth(
    data = make_map_sf(),
    fill_col = "event_count",
    map_type = "count"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 03. RATE MAP
# ============================================================================

test_that("map_risk_choropleth supports rate data", {

  p <- map_risk_choropleth(
    data = make_map_sf(),
    fill_col = "rate",
    map_type = "rate"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 04. PROBABILITY MAP
# ============================================================================

test_that("map_risk_choropleth supports probability data", {

  p <- map_risk_choropleth(
    data = make_map_sf(),
    fill_col = "probability",
    map_type = "probability"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 05. SMR MAP
# ============================================================================

test_that("map_risk_choropleth supports default SMR classification", {

  p <- map_risk_choropleth(
    data = make_map_sf(),
    fill_col = "smr",
    map_type = "smr"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 06. CATEGORY MAP
# ============================================================================

test_that("map_risk_choropleth supports categorical maps", {

  p <- map_risk_choropleth(
    data = make_map_sf(),
    fill_col = "category",
    map_type = "category"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 07. CONTINUOUS NUMERIC MAP
# ============================================================================

test_that("classification none produces numeric continuous map", {

  p <- map_risk_choropleth(
    data = make_map_sf(),
    fill_col = "rate",
    map_type = "rate",
    classification = "none"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 08. ALTERNATIVE CLASSIFICATIONS
# ============================================================================

test_that("alternative classification methods return ggplot", {

  x <- make_map_sf()

  for (
    method in c(
      "equal_interval",
      "jenks",
      "pretty"
    )
  ) {

    p <- map_risk_choropleth(
      data = x,
      fill_col = "rate",
      map_type = "rate",
      classification = method,
      n_classes = 3
    )

    expect_s3_class(
      p,
      "ggplot"
    )
  }
})


# ============================================================================
# 09. SINGLE UNIQUE VALUE
# ============================================================================

test_that("single unique numeric value is handled", {

  x <- make_map_sf()
  x$rate <- 1

  p <- map_risk_choropleth(
    data = x,
    fill_col = "rate",
    map_type = "rate"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 10. ALL NA
# ============================================================================

test_that("all missing numeric values are handled", {

  x <- make_map_sf()
  x$rate <- NA_real_

  p <- map_risk_choropleth(
    data = x,
    fill_col = "rate",
    map_type = "rate"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


# ============================================================================
# 11. NON-SF INPUT
# ============================================================================

test_that("map_risk_choropleth rejects non-sf input", {

  expect_error(
    map_risk_choropleth(
      data = data.frame(
        x = 1
      ),
      fill_col = "x"
    ),
    "must be an sf object"
  )
})


# ============================================================================
# 12. MISSING FILL COLUMN
# ============================================================================

test_that("map_risk_choropleth rejects missing fill column", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "missing"
    ),
    "not found"
  )
})


# ============================================================================
# 13. CATEGORY MUST BE CATEGORICAL
# ============================================================================

test_that("category maps reject numeric fill data", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "event_count",
      map_type = "category"
    ),
    "character or factor"
  )
})


# ============================================================================
# 14. NUMERIC MAP TYPES REQUIRE NUMERIC DATA
# ============================================================================

test_that("numeric map types reject character data", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "category",
      map_type = "rate"
    ),
    "numeric"
  )
})


# ============================================================================
# 15. NEGATIVE VALUES
# ============================================================================

test_that("risk numeric maps reject negative values", {

  x <- make_map_sf()
  x$rate[1] <- -1

  expect_error(
    map_risk_choropleth(
      data = x,
      fill_col = "rate",
      map_type = "rate"
    ),
    "non-negative"
  )
})


# ============================================================================
# 16. PROBABILITY BOUNDS
# ============================================================================

test_that("probability maps enforce zero to one bounds", {

  x <- make_map_sf()
  x$probability[1] <- 1.2

  expect_error(
    map_risk_choropleth(
      data = x,
      fill_col = "probability",
      map_type = "probability"
    ),
    "between 0 and 1"
  )
})


# ============================================================================
# 17. INFINITE VALUES
# ============================================================================

test_that("map_risk_choropleth rejects infinite numeric values", {

  x <- make_map_sf()
  x$rate[1] <- Inf

  expect_error(
    map_risk_choropleth(
      data = x,
      fill_col = "rate",
      map_type = "rate"
    ),
    "infinite"
  )
})


# ============================================================================
# 18. SMR CLASSIFICATION RESTRICTED TO SMR
# ============================================================================

test_that("smr_default classification is restricted to SMR maps", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "rate",
      map_type = "rate",
      classification = "smr_default"
    ),
    "only valid"
  )
})


# ============================================================================
# 19. CATEGORY CLASSIFICATION RESTRICTED
# ============================================================================

test_that("category map uses unclassified categorical mapping", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "category",
      map_type = "category",
      classification = "quantile"
    ),
    "requires"
  )
})


# ============================================================================
# 20. INVALID N CLASSES
# ============================================================================

test_that("map_risk_choropleth validates n_classes", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "rate",
      n_classes = 1
    ),
    "whole number"
  )

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "rate",
      n_classes = 2.5
    ),
    "whole number"
  )
})


# ============================================================================
# 21. INVALID PALETTE
# ============================================================================

test_that("map_risk_choropleth validates palette names", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "rate",
      palette = "DefinitelyNotAPalette"
    ),
    "Unknown RColorBrewer palette"
  )
})


# ============================================================================
# 22. INVALID REVERSE PALETTE
# ============================================================================

test_that("map_risk_choropleth validates reverse_palette", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "rate",
      reverse_palette = NA
    ),
    "TRUE or FALSE"
  )
})


# ============================================================================
# 23. INVALID BORDER WIDTH
# ============================================================================

test_that("map_risk_choropleth validates border_width", {

  expect_error(
    map_risk_choropleth(
      data = make_map_sf(),
      fill_col = "rate",
      border_width = -1
    ),
    "non-negative"
  )
})


# ============================================================================
# 24. CUSTOM LABELS
# ============================================================================

test_that("map_risk_choropleth accepts titles and legend labels", {

  p <- map_risk_choropleth(
    data = make_map_sf(),
    fill_col = "rate",
    map_type = "rate",
    title = "Risk rate",
    subtitle = "Synthetic example",
    fill_label = "Rate"
  )

  expect_equal(
    p$labels$title,
    "Risk rate"
  )

  expect_equal(
    p$labels$subtitle,
    "Synthetic example"
  )

  expect_equal(
    p$labels$fill,
    "Rate"
  )
})


# ============================================================================
# 25. DERIVED CLASS COLUMN COLLISION
# ============================================================================

test_that("map_risk_choropleth rejects derived class-column collision", {

  x <- make_map_sf()
  x$rate_class <- "existing"

  expect_error(
    map_risk_choropleth(
      data = x,
      fill_col = "rate",
      map_type = "rate"
    ),
    "already exists"
  )
})
