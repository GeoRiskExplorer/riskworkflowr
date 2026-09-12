# ============================================================================
# test-counts-integration.R
# Purpose: High-coverage contracts for the spatial count-building family
# ============================================================================


# ============================================================================
# 01. FIXTURES
# ============================================================================

make_count_units <- function() {

  sf::st_sf(
    unit_id = c(
      "A",
      "B",
      "C",
      "D"
    ),
    unit_name = c(
      "Area A",
      "Area B",
      "Area C",
      "Area D"
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
      sf::st_polygon(
        list(
          rbind(
            c(600, 0),
            c(700, 0),
            c(700, 100),
            c(600, 100),
            c(600, 0)
          )
        )
      ),
      crs = 3857
    )
  )
}


make_count_points <- function() {

  sf::st_sf(
    event_id = paste0(
      "E",
      sprintf(
        "%02d",
        1:8
      )
    ),
    geometry = sf::st_sfc(
      sf::st_point(c(20, 20)),    # A intersect
      sf::st_point(c(40, 40)),    # A intersect
      sf::st_point(c(220, 20)),   # B intersect
      sf::st_point(c(240, 40)),   # B intersect
      sf::st_point(c(280, 70)),   # B intersect
      sf::st_point(c(320, 50)),   # 20 m from B
      sf::st_point(c(900, 50)),   # 200 m from D
      sf::st_point(c(-200, 50)),  # 200 m from A
      crs = 3857
    )
  )
}


# ============================================================================
# 02. risk_count_units — BASIC COUNTING
# ============================================================================

test_that("risk_count_units counts represented units", {

  data <- data.frame(
    unit_id = c(
      "A",
      "A",
      "B",
      "B",
      "B"
    )
  )

  out <- risk_count_units(
    data = data,
    unit_id_col = "unit_id"
  )

  expect_equal(
    out$event_count[
      out$unit_id == "A"
    ],
    2
  )

  expect_equal(
    out$event_count[
      out$unit_id == "B"
    ],
    3
  )

  expect_equal(
    sum(out$event_count),
    5
  )
})


# ============================================================================
# 03. risk_count_units — UNMATCHED EXCLUDED
# ============================================================================

test_that("risk_count_units excludes unmatched records", {

  data <- data.frame(
    unit_id = c(
      "A",
      NA,
      "A",
      NA,
      "B"
    )
  )

  out <- risk_count_units(
    data = data,
    unit_id_col = "unit_id"
  )

  expect_equal(
    sum(out$event_count),
    3
  )

  expect_false(
    any(
      is.na(out$unit_id)
    )
  )
})


# ============================================================================
# 04. risk_count_units — NAME GROUPING
# ============================================================================

test_that("risk_count_units retains unit names", {

  data <- data.frame(
    unit_id = c(
      "A",
      "A",
      "B"
    ),
    unit_name = c(
      "Area A",
      "Area A",
      "Area B"
    )
  )

  out <- risk_count_units(
    data = data,
    unit_id_col = "unit_id",
    unit_name_col = "unit_name"
  )

  expect_true(
    "unit_name" %in%
      names(out)
  )

  expect_equal(
    out$unit_name[
      out$unit_id == "A"
    ],
    "Area A"
  )
})


# ============================================================================
# 05. risk_count_units — ZERO ROWS
# ============================================================================

test_that("risk_count_units supports zero-row input", {

  data <- data.frame(
    unit_id = character()
  )

  out <- risk_count_units(
    data = data,
    unit_id_col = "unit_id"
  )

  expect_equal(
    nrow(out),
    0
  )

  expect_true(
    "event_count" %in%
      names(out)
  )
})


# ============================================================================
# 06. risk_count_units — sf GEOMETRY DROP
# ============================================================================

test_that("risk_count_units drops sf geometry by default", {

  points <- make_count_points()[1:3, ]
  points$unit_id <- c(
    "A",
    "A",
    "B"
  )

  out <- risk_count_units(
    data = points,
    unit_id_col = "unit_id"
  )

  expect_false(
    inherits(
      out,
      "sf"
    )
  )
})


# ============================================================================
# 07. risk_count_units — CUSTOM COUNT NAME
# ============================================================================

test_that("risk_count_units supports a custom count column", {

  data <- data.frame(
    unit_id = c(
      "A",
      "A",
      "B"
    )
  )

  out <- risk_count_units(
    data = data,
    unit_id_col = "unit_id",
    count_col = "n_events"
  )

  expect_true(
    "n_events" %in%
      names(out)
  )
})


# ============================================================================
# 08. risk_join_counts — COMPLETE POPULATION
# ============================================================================

test_that("risk_join_counts retains all spatial units", {

  units <- make_count_units()

  counts <- data.frame(
    unit_id = c(
      "A",
      "B"
    ),
    event_count = c(
      2L,
      4L
    )
  )

  out <- risk_join_counts(
    units = units,
    counts = counts,
    unit_id_col = "unit_id"
  )

  expect_equal(
    nrow(out),
    4
  )

  expect_equal(
    out$event_count,
    c(
      2L,
      4L,
      0L,
      0L
    )
  )
})


# ============================================================================
# 09. risk_join_counts — UNIT ORDER
# ============================================================================

test_that("risk_join_counts preserves authoritative unit order", {

  units <- make_count_units()

  counts <- data.frame(
    unit_id = c(
      "B",
      "A"
    ),
    event_count = c(
      4L,
      2L
    )
  )

  out <- risk_join_counts(
    units = units,
    counts = counts,
    unit_id_col = "unit_id"
  )

  expect_equal(
    out$unit_id,
    units$unit_id
  )
})


# ============================================================================
# 10. risk_join_counts — GEOMETRY
# ============================================================================

test_that("risk_join_counts preserves geometry and CRS", {

  units <- make_count_units()

  counts <- data.frame(
    unit_id = "A",
    event_count = 2L
  )

  out <- risk_join_counts(
    units = units,
    counts = counts,
    unit_id_col = "unit_id"
  )

  expect_s3_class(
    out,
    "sf"
  )

  expect_equal(
    sf::st_geometry(out),
    sf::st_geometry(units)
  )

  expect_equal(
    sf::st_crs(out),
    sf::st_crs(units)
  )
})


# ============================================================================
# 11. risk_join_counts — NULL MISSING VALUE
# ============================================================================

test_that("risk_join_counts can preserve missing counts", {

  units <- make_count_units()

  counts <- data.frame(
    unit_id = "A",
    event_count = 2L
  )

  out <- risk_join_counts(
    units = units,
    counts = counts,
    unit_id_col = "unit_id",
    missing_count_value = NULL
  )

  expect_equal(
    out$event_count[1],
    2
  )

  expect_true(
    all(
      is.na(
        out$event_count[2:4]
      )
    )
  )
})


# ============================================================================
# 12. risk_join_counts — DUPLICATE COUNT IDS
# ============================================================================

test_that("risk_join_counts rejects duplicate count rows", {

  counts <- data.frame(
    unit_id = c(
      "A",
      "A"
    ),
    event_count = c(
      1,
      2
    )
  )

  expect_error(
    risk_join_counts(
      units = make_count_units(),
      counts = counts,
      unit_id_col = "unit_id"
    ),
    "at most one row"
  )
})


# ============================================================================
# 13. risk_join_counts — DUPLICATE UNIT IDS
# ============================================================================

test_that("risk_join_counts rejects duplicate authoritative unit ids", {

  units <- make_count_units()
  units$unit_id[2] <- "A"

  counts <- data.frame(
    unit_id = "A",
    event_count = 2
  )

  expect_error(
    risk_join_counts(
      units = units,
      counts = counts,
      unit_id_col = "unit_id"
    ),
    "uniquely identify"
  )
})


# ============================================================================
# 14. risk_join_counts — UNKNOWN COUNT ID
# ============================================================================

test_that("risk_join_counts rejects unknown unit identifiers", {

  counts <- data.frame(
    unit_id = "Z",
    event_count = 2
  )

  expect_error(
    risk_join_counts(
      units = make_count_units(),
      counts = counts,
      unit_id_col = "unit_id"
    ),
    "not present"
  )
})


# ============================================================================
# 15. risk_join_counts — NEGATIVE COUNT
# ============================================================================

test_that("risk_join_counts rejects negative counts", {

  counts <- data.frame(
    unit_id = "A",
    event_count = -1
  )

  expect_error(
    risk_join_counts(
      units = make_count_units(),
      counts = counts,
      unit_id_col = "unit_id"
    ),
    "negative"
  )
})


# ============================================================================
# 16. risk_join_counts — FRACTIONAL COUNT
# ============================================================================

test_that("risk_join_counts rejects fractional record counts", {

  counts <- data.frame(
    unit_id = "A",
    event_count = 1.5
  )

  expect_error(
    risk_join_counts(
      units = make_count_units(),
      counts = counts,
      unit_id_col = "unit_id"
    ),
    "whole-number"
  )
})


# ============================================================================
# 17. risk_join_counts — EXPLICIT NA COUNT
# ============================================================================

test_that("risk_join_counts rejects missing values inside the count table", {

  counts <- data.frame(
    unit_id = "A",
    event_count = NA_real_
  )

  expect_error(
    risk_join_counts(
      units = make_count_units(),
      counts = counts,
      unit_id_col = "unit_id"
    ),
    "cannot contain missing"
  )
})


# ============================================================================
# 18. risk_join_counts — NAME CONSISTENCY
# ============================================================================

test_that("risk_join_counts validates unit-name consistency", {

  counts <- data.frame(
    unit_id = "A",
    unit_name = "Wrong name",
    event_count = 2
  )

  expect_error(
    risk_join_counts(
      units = make_count_units(),
      counts = counts,
      unit_id_col = "unit_id",
      unit_name_col = "unit_name"
    ),
    "inconsistent"
  )
})


# ============================================================================
# 19. risk_build_counts — INTERSECT ONLY
# ============================================================================

test_that("risk_build_counts reconciles strict intersect counts", {

  out <- risk_build_counts(
    points = make_count_points(),
    units = make_count_units(),
    unit_id_col = "unit_id",
    unit_name_col = "unit_name",
    join_mode = "intersect",
    return = "all"
  )

  expect_equal(
    nrow(out$joined_points),
    8
  )

  expect_equal(
    sum(
      !is.na(
        out$joined_points$unit_id
      )
    ),
    5
  )

  expect_equal(
    sum(out$counts$event_count),
    5
  )

  expect_equal(
    out$units$event_count,
    c(
      2L,
      3L,
      0L,
      0L
    )
  )
})


# ============================================================================
# 20. risk_build_counts — INTERSECT + NEAREST
# ============================================================================

test_that("risk_build_counts includes controlled nearest fallbacks", {

  out <- risk_build_counts(
    points = make_count_points(),
    units = make_count_units(),
    unit_id_col = "unit_id",
    unit_name_col = "unit_name",
    join_mode = "intersect_nearest",
    max_distance_m = 50,
    return = "all"
  )

  expect_equal(
    sum(
      out$joined_points$join_method ==
        "intersect"
    ),
    5
  )

  expect_equal(
    sum(
      out$joined_points$join_method ==
        "nearest"
    ),
    1
  )

  expect_equal(
    sum(
      out$joined_points$join_method ==
        "unmatched"
    ),
    2
  )

  expect_equal(
    sum(out$counts$event_count),
    6
  )

  expect_equal(
    out$units$event_count,
    c(
      2L,
      4L,
      0L,
      0L
    )
  )
})


# ============================================================================
# 21. risk_build_counts — UNRESTRICTED NEAREST
# ============================================================================

test_that("risk_build_counts can assign all points by unrestricted nearest", {

  out <- risk_build_counts(
    points = make_count_points(),
    units = make_count_units(),
    unit_id_col = "unit_id",
    join_mode = "nearest",
    return = "all"
  )

  expect_equal(
    sum(
      !is.na(
        out$joined_points$unit_id
      )
    ),
    8
  )

  expect_equal(
    sum(out$counts$event_count),
    8
  )
})


# ============================================================================
# 22. risk_build_counts — ZERO EVENT INPUT
# ============================================================================

test_that("risk_build_counts supports no event records", {

  points <-
    make_count_points()[0, ]

  out <- risk_build_counts(
    points = points,
    units = make_count_units(),
    unit_id_col = "unit_id",
    unit_name_col = "unit_name",
    join_mode = "intersect_nearest",
    max_distance_m = 50,
    return = "all"
  )

  expect_equal(
    nrow(out$joined_points),
    0
  )

  expect_equal(
    nrow(out$counts),
    0
  )

  expect_equal(
    out$units$event_count,
    rep(
      0L,
      4
    )
  )
})


# ============================================================================
# 23. risk_build_counts — ONE EVENT
# ============================================================================

test_that("risk_build_counts supports a single event", {

  points <-
    make_count_points()[1, ]

  out <- risk_build_counts(
    points = points,
    units = make_count_units(),
    unit_id_col = "unit_id",
    join_mode = "intersect",
    return = "all"
  )

  expect_equal(
    sum(out$units$event_count),
    1
  )

  expect_equal(
    out$units$event_count[1],
    1
  )
})


# ============================================================================
# 24. risk_build_counts — DEFAULT RETURN
# ============================================================================

test_that("risk_build_counts returns counted sf units by default", {

  out <- risk_build_counts(
    points = make_count_points(),
    units = make_count_units(),
    unit_id_col = "unit_id",
    join_mode = "intersect"
  )

  expect_s3_class(
    out,
    "sf"
  )

  expect_true(
    "event_count" %in%
      names(out)
  )
})


# ============================================================================
# 25. END-TO-END RECONCILIATION CONTRACT
# ============================================================================

test_that("assigned event population exactly reconciles with unit counts", {

  out <- risk_build_counts(
    points = make_count_points(),
    units = make_count_units(),
    unit_id_col = "unit_id",
    unit_name_col = "unit_name",
    join_mode = "intersect_nearest",
    max_distance_m = 50,
    return = "all"
  )

  assigned_n <-
    sum(
      !is.na(
        out$joined_points$unit_id
      )
    )

  represented_count_n <-
    sum(
      out$counts$event_count
    )

  complete_count_n <-
    sum(
      out$units$event_count
    )

  expect_equal(
    assigned_n,
    6
  )

  expect_equal(
    represented_count_n,
    assigned_n
  )

  expect_equal(
    complete_count_n,
    assigned_n
  )

  expect_equal(
    nrow(out$joined_points),
    nrow(make_count_points())
  )

  expect_equal(
    out$joined_points$event_id,
    make_count_points()$event_id
  )
})
