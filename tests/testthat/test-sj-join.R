# ============================================================================
# test-sj-join.R
# Purpose: High-coverage spatial assignment contract tests for sj_join()
# ============================================================================


# ============================================================================
# 01. FIXTURES
# ============================================================================

make_test_polygons <- function() {

  sf::st_sf(
    unit_id = c("A", "B", "C"),
    unit_name = c(
      "Area A",
      "Area B",
      "Area C"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          rbind(
            c(0, 0),
            c(100, 0),
            c(100, 100),
            c(0, 100),
            c(0, 0)
          )
        )
      ),
      sf::st_polygon(
        list(
          rbind(
            c(200, 0),
            c(300, 0),
            c(300, 100),
            c(200, 100),
            c(200, 0)
          )
        )
      ),
      sf::st_polygon(
        list(
          rbind(
            c(400, 0),
            c(500, 0),
            c(500, 100),
            c(400, 100),
            c(400, 0)
          )
        )
      ),
      crs = 3857
    )
  )
}


make_test_points <- function() {

  sf::st_sf(
    event_id = 1:4,
    value = c(10, 20, 30, 40),
    geometry = sf::st_sfc(
      sf::st_point(c(50, 50)),    # inside A
      sf::st_point(c(250, 50)),   # inside B
      sf::st_point(c(350, 50)),   # 50 m from B/C
      sf::st_point(c(700, 50)),   # 200 m from C
      crs = 3857
    )
  )
}


# ============================================================================
# 02. STRICT INTERSECT — INSIDE POINTS
# ============================================================================

test_that("sj_join assigns clearly intersecting points", {

  polygons <- make_test_polygons()
  points <- make_test_points()[1:2, ]

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    name_col = "unit_name",
    join_mode = "intersect"
  )

  expect_equal(
    out$unit_id,
    c("A", "B")
  )

  expect_equal(
    out$unit_name,
    c("Area A", "Area B")
  )

  expect_equal(
    out$join_method,
    c("intersect", "intersect")
  )

  expect_equal(
    out$join_distance_m,
    c(0, 0)
  )
})


# ============================================================================
# 03. STRICT INTERSECT — OUTSIDE POINT
# ============================================================================

test_that("strict intersect leaves outside points unmatched", {

  polygons <- make_test_polygons()
  points <- make_test_points()[3, ]

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect"
  )

  expect_true(
    is.na(out$unit_id)
  )

  expect_equal(
    out$join_method,
    "unmatched"
  )

  expect_true(
    is.na(out$join_distance_m)
  )
})


# ============================================================================
# 04. INTERSECT-NEAREST — MIXED CONDITIONS
# ============================================================================

test_that("intersect_nearest uses intersect first and nearest only as fallback", {

  polygons <- make_test_polygons()
  points <- make_test_points()[1:3, ]

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    out$join_method,
    c(
      "intersect",
      "intersect",
      "nearest"
    )
  )

  expect_equal(
    out$join_distance_m[1:2],
    c(0, 0)
  )

  expect_equal(
    out$join_distance_m[3],
    50,
    tolerance = 1e-8
  )
})


# ============================================================================
# 05. NEAREST MODE
# ============================================================================

test_that("nearest mode assigns every eligible point using nearest", {

  polygons <- make_test_polygons()
  points <- make_test_points()

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "nearest"
  )

  expect_true(
    all(
      out$join_method ==
        "nearest"
    )
  )

  expect_equal(
    nrow(out),
    nrow(points)
  )

  expect_true(
    all(
      !is.na(out$unit_id)
    )
  )
})


# ============================================================================
# 06. NEAREST DISTANCE
# ============================================================================

test_that("nearest distance is calculated correctly", {

  polygons <- make_test_polygons()

  points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_point(
        c(150, 50)
      ),
      crs = 3857
    )
  )

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "nearest"
  )

  expect_equal(
    out$join_distance_m,
    50,
    tolerance = 1e-8
  )
})


# ============================================================================
# 07. MAXIMUM DISTANCE — ACCEPT
# ============================================================================

test_that("nearest assignment occurs within maximum distance", {

  polygons <- make_test_polygons()

  points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_point(
        c(150, 50)
      ),
      crs = 3857
    )
  )

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest",
    max_distance_m = 50
  )

  expect_equal(
    out$join_method,
    "nearest"
  )

  expect_false(
    is.na(out$unit_id)
  )
})


# ============================================================================
# 08. MAXIMUM DISTANCE — REJECT
# ============================================================================

test_that("nearest assignment is rejected beyond maximum distance", {

  polygons <- make_test_polygons()

  points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_point(
        c(700, 50)
      ),
      crs = 3857
    )
  )

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest",
    max_distance_m = 100
  )

  expect_true(
    is.na(out$unit_id)
  )

  expect_equal(
    out$join_method,
    "unmatched"
  )

  expect_equal(
    out$join_distance_m,
    200,
    tolerance = 1e-8
  )
})


# ============================================================================
# 09. MAXIMUM DISTANCE BOUNDARY
# ============================================================================

test_that("point exactly at maximum distance is accepted", {

  polygons <- make_test_polygons()

  points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_point(
        c(150, 50)
      ),
      crs = 3857
    )
  )

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "nearest",
    max_distance_m = 50
  )

  expect_equal(
    out$join_method,
    "nearest"
  )
})


# ============================================================================
# 10. POINT ON EXTERNAL POLYGON BOUNDARY
# ============================================================================

test_that("a point on an external polygon boundary intersects that polygon", {

  polygons <- make_test_polygons()[1, ]

  points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_point(
        c(0, 50)
      ),
      crs = 3857
    )
  )

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect"
  )

  expect_equal(
    out$unit_id,
    "A"
  )

  expect_equal(
    out$join_method,
    "intersect"
  )

  expect_equal(
    out$join_distance_m,
    0
  )
})


# ============================================================================
# 11. SHARED BOUNDARY AMBIGUITY
# ============================================================================

test_that("shared-boundary intersections are rejected as ambiguous", {

  polygons <- sf::st_sf(
    unit_id = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          rbind(
            c(0, 0),
            c(100, 0),
            c(100, 100),
            c(0, 100),
            c(0, 0)
          )
        )
      ),
      sf::st_polygon(
        list(
          rbind(
            c(100, 0),
            c(200, 0),
            c(200, 100),
            c(100, 100),
            c(100, 0)
          )
        )
      ),
      crs = 3857
    )
  )

  points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_point(
        c(100, 50)
      ),
      crs = 3857
    )
  )

  expect_error(
    sj_join(
      points = points,
      polygons = polygons,
      id_col = "unit_id",
      join_mode = "intersect"
    ),
    "ambiguous"
  )
})


# ============================================================================
# 12. OVERLAPPING POLYGONS
# ============================================================================

test_that("overlapping polygon matches do not silently duplicate events", {

  polygons <- sf::st_sf(
    unit_id = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          rbind(
            c(0, 0),
            c(150, 0),
            c(150, 150),
            c(0, 150),
            c(0, 0)
          )
        )
      ),
      sf::st_polygon(
        list(
          rbind(
            c(50, 50),
            c(200, 50),
            c(200, 200),
            c(50, 200),
            c(50, 50)
          )
        )
      ),
      crs = 3857
    )
  )

  points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_point(
        c(100, 100)
      ),
      crs = 3857
    )
  )

  expect_error(
    sj_join(
      points = points,
      polygons = polygons,
      id_col = "unit_id",
      join_mode = "intersect_nearest"
    ),
    "ambiguous"
  )
})


# ============================================================================
# 13. ROW COUNT PRESERVATION
# ============================================================================

test_that("sj_join preserves one output row per input event", {

  polygons <- make_test_polygons()
  points <- make_test_points()

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    nrow(out),
    nrow(points)
  )
})


# ============================================================================
# 14. ROW ORDER PRESERVATION
# ============================================================================

test_that("sj_join preserves input event order", {

  polygons <- make_test_polygons()
  points <- make_test_points()

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    out$event_id,
    points$event_id
  )
})


# ============================================================================
# 15. ATTRIBUTE PRESERVATION
# ============================================================================

test_that("sj_join preserves original point attributes", {

  polygons <- make_test_polygons()
  points <- make_test_points()

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    out$value,
    points$value
  )

  expect_equal(
    out$event_id,
    points$event_id
  )
})


# ============================================================================
# 16. GEOMETRY PRESERVATION
# ============================================================================

test_that("sj_join preserves point geometry", {

  polygons <- make_test_polygons()
  points <- make_test_points()

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    sf::st_geometry(out),
    sf::st_geometry(points)
  )
})


# ============================================================================
# 17. CRS PRESERVATION
# ============================================================================

test_that("sj_join preserves point CRS", {

  polygons <- make_test_polygons()
  points <- make_test_points()

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    sf::st_crs(out),
    sf::st_crs(points)
  )
})


# ============================================================================
# 18. OPTIONAL NAME COLUMN
# ============================================================================

test_that("sj_join transfers optional unit names", {

  polygons <- make_test_polygons()
  points <- make_test_points()[1, ]

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    name_col = "unit_name",
    join_mode = "intersect"
  )

  expect_equal(
    out$unit_name,
    "Area A"
  )
})


# ============================================================================
# 19. EMPTY POINT DATASET
# ============================================================================

test_that("sj_join supports zero-row point input", {

  polygons <- make_test_polygons()

  points <- sf::st_sf(
    event_id = integer(),
    geometry = sf::st_sfc(
      crs = 3857
    )
  )

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    nrow(out),
    0
  )

  expect_true(
    "unit_id" %in%
      names(out)
  )

  expect_true(
    "join_method" %in%
      names(out)
  )

  expect_true(
    "join_distance_m" %in%
      names(out)
  )
})


# ============================================================================
# 20. EMPTY POLYGON DATASET
# ============================================================================

test_that("sj_join rejects empty polygon input", {

  polygons <-
    make_test_polygons()[0, ]

  points <-
    make_test_points()[1, ]

  expect_error(
    sj_join(
      points = points,
      polygons = polygons,
      id_col = "unit_id"
    ),
    "at least one feature"
  )
})


# ============================================================================
# 21. NON-SF POINT INPUT
# ============================================================================

test_that("sj_join rejects non-sf point input", {

  expect_error(
    sj_join(
      points = data.frame(
        x = 1,
        y = 1
      ),
      polygons = make_test_polygons(),
      id_col = "unit_id"
    ),
    "must be an sf object"
  )
})


# ============================================================================
# 22. NON-SF POLYGON INPUT
# ============================================================================

test_that("sj_join rejects non-sf polygon input", {

  expect_error(
    sj_join(
      points = make_test_points(),
      polygons = data.frame(
        unit_id = "A"
      ),
      id_col = "unit_id"
    ),
    "must be an sf object"
  )
})


# ============================================================================
# 23. WRONG POINT GEOMETRY TYPE
# ============================================================================

test_that("sj_join rejects non-point event geometry", {

  polygons <- make_test_polygons()

  wrong_points <- sf::st_sf(
    event_id = 1,
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(
          c(
            0, 0,
            10, 10
          ),
          ncol = 2,
          byrow = TRUE
        )
      ),
      crs = 3857
    )
  )

  expect_error(
    sj_join(
      points = wrong_points,
      polygons = polygons,
      id_col = "unit_id"
    ),
    "POINT geometries"
  )
})


# ============================================================================
# 24. WRONG POLYGON GEOMETRY TYPE
# ============================================================================

test_that("sj_join rejects non-polygon target geometry", {

  points <- make_test_points()

  wrong_polygons <- sf::st_sf(
    unit_id = "A",
    geometry = sf::st_sfc(
      sf::st_point(
        c(0, 0)
      ),
      crs = 3857
    )
  )

  expect_error(
    sj_join(
      points = points,
      polygons = wrong_polygons,
      id_col = "unit_id"
    ),
    "POLYGON or MULTIPOLYGON"
  )
})


# ============================================================================
# 25. CRS MISMATCH
# ============================================================================

test_that("sj_join rejects CRS mismatch", {

  points <- make_test_points()

  polygons <-
    sf::st_transform(
      make_test_polygons(),
      4326
    )

  expect_error(
    sj_join(
      points = points,
      polygons = polygons,
      id_col = "unit_id"
    ),
    "same CRS"
  )
})


# ============================================================================
# 26. MISSING POINT CRS
# ============================================================================

test_that("sj_join rejects missing point CRS", {

  points <- make_test_points()

  sf::st_crs(points) <- NA

  expect_error(
    sj_join(
      points = points,
      polygons = make_test_polygons(),
      id_col = "unit_id"
    ),
    "defined CRS"
  )
})


# ============================================================================
# 27. MISSING POLYGON CRS
# ============================================================================

test_that("sj_join rejects missing polygon CRS", {

  polygons <- make_test_polygons()

  sf::st_crs(polygons) <- NA

  expect_error(
    sj_join(
      points = make_test_points(),
      polygons = polygons,
      id_col = "unit_id"
    ),
    "defined CRS"
  )
})


# ============================================================================
# 28. MISSING ID COLUMN
# ============================================================================

test_that("sj_join rejects missing polygon identifier column", {

  expect_error(
    sj_join(
      points = make_test_points(),
      polygons = make_test_polygons(),
      id_col = "missing_id"
    ),
    "not found"
  )
})


# ============================================================================
# 29. MISSING NAME COLUMN
# ============================================================================

test_that("sj_join rejects missing polygon name column", {

  expect_error(
    sj_join(
      points = make_test_points(),
      polygons = make_test_polygons(),
      id_col = "unit_id",
      name_col = "missing_name"
    ),
    "not found"
  )
})


# ============================================================================
# 30. MISSING POLYGON IDENTIFIERS
# ============================================================================

test_that("sj_join rejects missing polygon identifiers", {

  polygons <- make_test_polygons()

  polygons$unit_id[2] <- NA_character_

  expect_error(
    sj_join(
      points = make_test_points(),
      polygons = polygons,
      id_col = "unit_id"
    ),
    "missing polygon identifiers"
  )
})


# ============================================================================
# 31. DUPLICATE POLYGON IDENTIFIERS
# ============================================================================

test_that("sj_join rejects duplicate polygon identifiers", {

  polygons <- make_test_polygons()

  polygons$unit_id[2] <- "A"

  expect_error(
    sj_join(
      points = make_test_points(),
      polygons = polygons,
      id_col = "unit_id"
    ),
    "uniquely identify"
  )
})


# ============================================================================
# 32. OUTPUT COLUMN COLLISION — ID
# ============================================================================

test_that("sj_join rejects existing point ID output column", {

  points <- make_test_points()

  points$unit_id <- "existing"

  expect_error(
    sj_join(
      points = points,
      polygons = make_test_polygons(),
      id_col = "unit_id"
    ),
    "already exists"
  )
})


# ============================================================================
# 33. OUTPUT COLUMN COLLISION — PROVENANCE
# ============================================================================

test_that("sj_join rejects existing provenance columns", {

  points <- make_test_points()

  points$join_method <- "existing"

  expect_error(
    sj_join(
      points = points,
      polygons = make_test_polygons(),
      id_col = "unit_id"
    ),
    "already exists"
  )
})


# ============================================================================
# 34. INVALID MAXIMUM DISTANCE
# ============================================================================

test_that("sj_join validates maximum nearest distance", {

  points <- make_test_points()
  polygons <- make_test_polygons()

  expect_error(
    sj_join(
      points = points,
      polygons = polygons,
      id_col = "unit_id",
      max_distance_m = -1
    ),
    "non-negative"
  )

  expect_error(
    sj_join(
      points = points,
      polygons = polygons,
      id_col = "unit_id",
      max_distance_m = Inf
    ),
    "finite"
  )

  expect_error(
    sj_join(
      points = points,
      polygons = polygons,
      id_col = "unit_id",
      max_distance_m = NA_real_
    ),
    "NULL or one"
  )
})


# ============================================================================
# 35. INVALID JOIN MODE
# ============================================================================

test_that("sj_join rejects unsupported join modes", {

  expect_error(
    sj_join(
      points = make_test_points(),
      polygons = make_test_polygons(),
      id_col = "unit_id",
      join_mode = "banana"
    )
  )
})


# ============================================================================
# 36. LARGE SYNTHETIC ROW CONTRACT
# ============================================================================

test_that("sj_join preserves event population in a larger synthetic workflow", {

  set.seed(123)

  polygons <- make_test_polygons()

  coords <- cbind(
    runif(
      1000,
      min = -50,
      max = 550
    ),
    runif(
      1000,
      min = -50,
      max = 150
    )
  )

  points <- sf::st_as_sf(
    data.frame(
      event_id = seq_len(1000),
      x = coords[, 1],
      y = coords[, 2]
    ),
    coords = c("x", "y"),
    crs = 3857
  )

  out <- sj_join(
    points = points,
    polygons = polygons,
    id_col = "unit_id",
    join_mode = "intersect_nearest"
  )

  expect_equal(
    nrow(out),
    1000
  )

  expect_equal(
    out$event_id,
    seq_len(1000)
  )

  expect_true(
    all(
      out$join_method %in%
        c(
          "intersect",
          "nearest",
          "unmatched"
        )
    )
  )

  expect_false(
    anyDuplicated(out$event_id) > 0L
  )
})