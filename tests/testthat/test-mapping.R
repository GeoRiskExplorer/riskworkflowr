# 00001 — Load libraries ----------------------------------------------------

library(sf)

# 00002 — Create simple polygon test data -----------------------------------

map_test_data <- sf::st_sf(
  unit_id = LETTERS[1:6],
  event_count = c(1, 2, 3, 4, 5, 6),
  rate_per_10000 = c(50, 100, 150, 200, 250, 300),
  smr = c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6),
  risk_category = c("Lower", "Lower", "Expected", "Expected", "Higher", "Higher"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))),
    sf::st_polygon(list(rbind(c(10, 0), c(20, 0), c(20, 10), c(10, 10), c(10, 0)))),
    sf::st_polygon(list(rbind(c(20, 0), c(30, 0), c(30, 10), c(20, 10), c(20, 0)))),
    sf::st_polygon(list(rbind(c(0, 10), c(10, 10), c(10, 20), c(0, 20), c(0, 10)))),
    sf::st_polygon(list(rbind(c(10, 10), c(20, 10), c(20, 20), c(10, 20), c(10, 10)))),
    sf::st_polygon(list(rbind(c(20, 10), c(30, 10), c(30, 20), c(20, 20), c(20, 10))))
  ),
  crs = 3857
)

# 00003 — Count map returns ggplot ------------------------------------------

test_that("map_risk_choropleth returns ggplot for count map", {

  result <- map_risk_choropleth(
    data = map_test_data,
    fill_col = "event_count",
    map_type = "count",
     n_classes = 5,
    title = "Test count map"
  )

  expect_s3_class(result, "ggplot")

})

# 00004 — Rate map returns ggplot -------------------------------------------

test_that("map_risk_choropleth returns ggplot for rate map", {

  result <- map_risk_choropleth(
    data = map_test_data,
    fill_col = "rate_per_10000",
    map_type = "rate",
    n_classes = 5,
    title = "Test rate map"
  )

  expect_s3_class(result, "ggplot")

})

# 00005 — SMR map returns ggplot --------------------------------------------

test_that("map_risk_choropleth returns ggplot for smr map", {

  result <- map_risk_choropleth(
    data = map_test_data,
    fill_col = "smr",
    map_type = "smr",
    title = "Test SMR map"
  )

  expect_s3_class(result, "ggplot")

})

# 00006 — Category map returns ggplot ---------------------------------------

test_that("map_risk_choropleth returns ggplot for category map", {

  result <- map_risk_choropleth(
    data = map_test_data,
    fill_col = "risk_category",
    map_type = "category",
    title = "Test category map"
  )

  expect_s3_class(result, "ggplot")

})