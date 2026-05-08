# 00001 — Load libraries ----------------------------------------------------

library(sf)

# 00002 — Create simple polygon test data -----------------------------------

test_polygons <- sf::st_sf(
  unit_id = c("A", "B"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(10, 0),
      c(10, 10),
      c(0, 10),
      c(0, 0)
    ))),
    sf::st_polygon(list(rbind(
      c(10, 0),
      c(20, 0),
      c(20, 10),
      c(10, 10),
      c(10, 0)
    )))
  ),
  crs = 3857
)

# 00003 — Create simple point test data -------------------------------------

test_points <- sf::st_sf(
  point_id = c(1, 2, 3),
  geometry = sf::st_sfc(
    sf::st_point(c(5, 5)),
    sf::st_point(c(15, 5)),
    sf::st_point(c(30, 30))
  ),
  crs = 3857
)

# 00004 — Test sj_join() intersect mode -------------------------------------

test_that("sj_join intersect mode works", {

  result <- sj_join(
    points = test_points,
    polygons = test_polygons,
    id_col = "unit_id",
    join = "intersect"
  )

  expect_true(nrow(result) >= 2)
  expect_true("unit_id" %in% names(result))
  expect_true("join_method" %in% names(result))

})

# 00005 — Test sj_join() nearest mode ---------------------------------------

test_that("sj_join nearest mode works", {

  result <- sj_join(
    points = test_points,
    polygons = test_polygons,
    id_col = "unit_id",
    join = "nearest"
  )

  expect_equal(nrow(result), 3)
  expect_true("unit_id" %in% names(result))
  expect_true("join_method" %in% names(result))

})

# 00006 — Test risk_count_units() -------------------------------------------

test_that("risk_count_units returns counts", {

  joined <- sj_join(
    points = test_points,
    polygons = test_polygons,
    id_col = "unit_id",
    join = "nearest"
  )

  result <- risk_count_units(
    data = joined,
    unit_id_col = "unit_id"
  )

  expect_true(nrow(result) > 0)
  expect_true("unit_id" %in% names(result))
  expect_true("event_count" %in% names(result))

})

# 00007 — Test geom_repair() ------------------------------------------------

test_that("geom_repair returns sf object", {

  result <- geom_repair(test_polygons)

  expect_true(inherits(result, "sf"))
  expect_equal(nrow(result), nrow(test_polygons))

})

# 00008 — Test risk_build_counts() ------------------------------------------

test_that("risk_build_counts builds counts and joins them to units", {

  result <- risk_build_counts(
    points = test_points,
    units = test_polygons,
    unit_id_col = "unit_id",
    join_mode = "nearest"
  )

  expect_true(inherits(result, "sf"))
  expect_equal(nrow(result), 2)
  expect_true("unit_id" %in% names(result))
  expect_true("event_count" %in% names(result))

  expect_equal(sum(result$event_count), 3)

})

# 00009 — Test risk_join_counts() -------------------------------------------

test_that("risk_join_counts joins counts back to units", {

  counts <- data.frame(
    unit_id = c("A", "B"),
    event_count = c(2, 1)
  )

  result <- risk_join_counts(
    units = test_polygons,
    counts = counts,
    unit_id_col = "unit_id"
  )

  expect_true(inherits(result, "sf"))
  expect_equal(nrow(result), 2)
  expect_true("event_count" %in% names(result))
  expect_equal(sum(result$event_count), 3)

})