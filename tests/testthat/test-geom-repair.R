# ============================================================================
# test-geom-repair.R
# ============================================================================


# ============================================================================
# 01. FIXTURES
# ============================================================================

make_valid_sf <- function() {

  sf::st_sf(
    id = 1:2,
    value = c(
      "A",
      "B"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          rbind(
            c(0, 0),
            c(10, 0),
            c(10, 10),
            c(0, 10),
            c(0, 0)
          )
        )
      ),
      sf::st_polygon(
        list(
          rbind(
            c(20, 0),
            c(30, 0),
            c(30, 10),
            c(20, 10),
            c(20, 0)
          )
        )
      ),
      crs = 3857
    )
  )
}


make_invalid_sf <- function() {

  sf::st_sf(
    id = 1L,
    value = "bowtie",
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          rbind(
            c(0, 0),
            c(10, 10),
            c(0, 10),
            c(10, 0),
            c(0, 0)
          )
        )
      ),
      crs = 3857
    )
  )
}


# ============================================================================
# 02. VALID INPUT REMAINS VALID
# ============================================================================

test_that("geom_repair preserves already-valid geometry", {

  x <- make_valid_sf()

  out <- geom_repair(x)

  expect_true(
    all(
      sf::st_is_valid(out)
    )
  )

  expect_equal(
    nrow(out),
    nrow(x)
  )
})


# ============================================================================
# 03. INVALID GEOMETRY IS REPAIRED
# ============================================================================

test_that("geom_repair repairs invalid geometry", {

  x <- make_invalid_sf()

  expect_false(
    all(
      sf::st_is_valid(x)
    )
  )

  out <- geom_repair(x)

  expect_true(
    all(
      sf::st_is_valid(out)
    )
  )
})


# ============================================================================
# 04. ATTRIBUTES PRESERVED
# ============================================================================

test_that("geom_repair preserves non-geometry attributes", {

  x <- make_invalid_sf()

  out <- geom_repair(x)

  expect_equal(
    out$id,
    x$id
  )

  expect_equal(
    out$value,
    x$value
  )
})


# ============================================================================
# 05. CRS PRESERVED
# ============================================================================

test_that("geom_repair preserves CRS", {

  x <- make_invalid_sf()

  out <- geom_repair(x)

  expect_identical(
    sf::st_crs(out),
    sf::st_crs(x)
  )
})


# ============================================================================
# 06. ROW COUNT PRESERVED
# ============================================================================

test_that("geom_repair preserves feature count", {

  x <- make_invalid_sf()

  out <- geom_repair(x)

  expect_equal(
    nrow(out),
    nrow(x)
  )
})


# ============================================================================
# 07. ACTIVE GEOMETRY COLUMN PRESERVED
# ============================================================================

test_that("geom_repair preserves active geometry column", {

  x <- make_invalid_sf()

  out <- geom_repair(x)

  expect_identical(
    attr(out, "sf_column"),
    attr(x, "sf_column")
  )
})


# ============================================================================
# 08. SUMMARY ATTRIBUTE
# ============================================================================

test_that("geom_repair attaches repair summary", {

  x <- make_invalid_sf()

  out <- geom_repair(x)

  summary <-
    attr(
      out,
      "geom_repair_summary"
    )

  expect_type(
    summary,
    "list"
  )

  expect_equal(
    summary$n_features,
    1L
  )

  expect_equal(
    summary$n_invalid_before,
    1L
  )

  expect_equal(
    summary$n_invalid_after,
    0L
  )

  expect_equal(
    summary$n_unknown_before,
    0L
  )

  expect_equal(
    summary$n_unknown_after,
    0L
  )
})


# ============================================================================
# 09. VALID GEOMETRY SUMMARY
# ============================================================================

test_that("geom_repair reports zero repairs for valid input", {

  out <- geom_repair(
    make_valid_sf()
  )

  summary <-
    attr(
      out,
      "geom_repair_summary"
    )

  expect_equal(
    summary$n_invalid_before,
    0L
  )

  expect_equal(
    summary$n_invalid_after,
    0L
  )
})


# ============================================================================
# 10. QUIET BY DEFAULT
# ============================================================================

test_that("geom_repair is quiet by default", {

  expect_silent(
    geom_repair(
      make_invalid_sf()
    )
  )
})


# ============================================================================
# 11. OPTIONAL MESSAGE
# ============================================================================

test_that("geom_repair can print concise QA summary", {

  expect_message(
    geom_repair(
      make_invalid_sf(),
      quiet = FALSE
    ),
    "Geometry repair summary"
  )
})


# ============================================================================
# 12. INVALID QUIET
# ============================================================================

test_that("geom_repair validates quiet", {

  expect_error(
    geom_repair(
      make_valid_sf(),
      quiet = NA
    ),
    "TRUE or FALSE"
  )

  expect_error(
    geom_repair(
      make_valid_sf(),
      quiet = "yes"
    ),
    "TRUE or FALSE"
  )
})


# ============================================================================
# 13. NON-SF INPUT
# ============================================================================

test_that("geom_repair rejects non-sf input", {

  expect_error(
    geom_repair(
      data.frame(
        x = 1
      )
    ),
    "must be an sf object"
  )
})


# ============================================================================
# 14. ZERO-ROW SF
# ============================================================================

test_that("geom_repair supports zero-row sf input", {

  x <- make_valid_sf()[0, ]

  out <- geom_repair(x)

  expect_s3_class(
    out,
    "sf"
  )

  expect_equal(
    nrow(out),
    0L
  )

  summary <-
    attr(
      out,
      "geom_repair_summary"
    )

  expect_equal(
    summary$n_features,
    0L
  )

  expect_equal(
    summary$n_invalid_before,
    0L
  )

  expect_equal(
    summary$n_invalid_after,
    0L
  )
})


# ============================================================================
# 15. EMPTY GEOMETRY
# ============================================================================

test_that("geom_repair supports empty geometry", {

  x <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_point(),
      crs = 3857
    )
  )

  out <- geom_repair(x)

  expect_s3_class(
    out,
    "sf"
  )

  expect_equal(
    nrow(out),
    1L
  )

  expect_true(
    sf::st_is_empty(
      out
    )
  )
})


# ============================================================================
# 16. MIXED VALID AND INVALID GEOMETRY
# ============================================================================

test_that("geom_repair repairs mixed valid and invalid features", {

  valid_poly <-
    sf::st_polygon(
      list(
        rbind(
          c(20, 0),
          c(30, 0),
          c(30, 10),
          c(20, 10),
          c(20, 0)
        )
      )
    )

  invalid_poly <-
    sf::st_polygon(
      list(
        rbind(
          c(0, 0),
          c(10, 10),
          c(0, 10),
          c(10, 0),
          c(0, 0)
        )
      )
    )

  x <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      invalid_poly,
      valid_poly,
      crs = 3857
    )
  )

  out <- geom_repair(x)

  expect_true(
    all(
      sf::st_is_valid(out)
    )
  )

  summary <-
    attr(
      out,
      "geom_repair_summary"
    )

  expect_equal(
    summary$n_invalid_before,
    1L
  )

  expect_equal(
    summary$n_invalid_after,
    0L
  )
})


# ============================================================================
# 17. CUSTOM GEOMETRY COLUMN NAME
# ============================================================================

test_that("geom_repair preserves custom geometry column name", {

  x <- make_invalid_sf()

  names(x)[
    names(x) == "geometry"
  ] <- "shape"

  sf::st_geometry(x) <- "shape"

  out <- geom_repair(x)

  expect_identical(
    attr(out, "sf_column"),
    "shape"
  )

  expect_true(
    all(
      sf::st_is_valid(out)
    )
  )
})
