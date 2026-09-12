# ============================================================================
# test-risk-distinct-category.R
# ============================================================================


# ============================================================================
# 01. FIXTURE
# ============================================================================

make_distinct_data <- function() {

  data.frame(
    unit_id = rep(
      c(
        "A",
        "B",
        "C"
      ),
      each = 3
    ),
    category = rep(
      c(
        "Falls",
        "Water",
        "Wildlife"
      ),
      times = 3
    ),
    event_count = c(
      10, 2, 1,
      3, 8, 1,
      2, 2, 8
    ),
    exposure = rep(
      c(
        1000,
        800,
        1200
      ),
      each = 3
    ),
    stringsAsFactors = FALSE
  )
}


# ============================================================================
# 02. BASIC OUTPUT
# ============================================================================

test_that("risk_distinct_category returns one row per unit", {

  out <- risk_distinct_category(
    data = make_distinct_data(),
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_equal(
    nrow(out),
    3
  )

  expect_equal(
    out$unit_id,
    c(
      "A",
      "B",
      "C"
    )
  )
})


# ============================================================================
# 03. EXPECTED DISTINCTIVE CATEGORIES
# ============================================================================

test_that("risk_distinct_category identifies expected highest categories", {

  out <- risk_distinct_category(
    data = make_distinct_data(),
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_equal(
    out$highest_category,
    c(
      "Falls",
      "Water",
      "Wildlife"
    )
  )
})


# ============================================================================
# 04. LOWEST CATEGORY
# ============================================================================

test_that("risk_distinct_category returns lowest categories by default", {

  out <- risk_distinct_category(
    data = make_distinct_data(),
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_true(
    all(
      c(
        "lowest_category",
        "lowest_smr",
        "lowest_event_count"
      ) %in%
        names(out)
    )
  )
})


# ============================================================================
# 05. LOWEST CATEGORY OPTIONAL
# ============================================================================

test_that("risk_distinct_category can omit lowest-category outputs", {

  out <- risk_distinct_category(
    data = make_distinct_data(),
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure",
    include_lowest = FALSE
  )

  expect_false(
    any(
      c(
        "lowest_category",
        "lowest_smr",
        "lowest_event_count"
      ) %in%
        names(out)
    )
  )
})


# ============================================================================
# 06. MINIMUM COUNT
# ============================================================================

test_that("risk_distinct_category applies minimum count threshold", {

  out <- risk_distinct_category(
    data = make_distinct_data(),
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure",
    min_count = 5
  )

  expect_equal(
    out$category_count_used,
    c(
      1L,
      1L,
      1L
    )
  )
})


# ============================================================================
# 07. INSUFFICIENT COUNT FLAG
# ============================================================================

test_that("risk_distinct_category flags units with no eligible category", {

  out <- risk_distinct_category(
    data = make_distinct_data(),
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure",
    min_count = 20
  )

  expect_true(
    all(
      out$insufficient_count_flag
    )
  )

  expect_true(
    all(
      is.na(
        out$highest_category
      )
    )
  )
})


# ============================================================================
# 08. CUSTOM OUTPUT NAMES
# ============================================================================

test_that("risk_distinct_category supports custom output names", {

  out <- risk_distinct_category(
    data = make_distinct_data(),
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure",
    highest_category_col = "top_category",
    highest_smr_col = "top_ratio",
    highest_count_col = "top_count"
  )

  expect_true(
    all(
      c(
        "top_category",
        "top_ratio",
        "top_count"
      ) %in%
        names(out)
    )
  )
})


# ============================================================================
# 09. DUPLICATE UNIT-CATEGORY KEYS
# ============================================================================

test_that("risk_distinct_category rejects duplicate unit-category rows", {

  x <- make_distinct_data()

  x <- rbind(
    x,
    x[1, ]
  )

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "at most one row"
  )
})


# ============================================================================
# 10. MISSING UNIT ID
# ============================================================================

test_that("risk_distinct_category rejects missing unit ids", {

  x <- make_distinct_data()
  x$unit_id[1] <- NA

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "unit_id_col"
  )
})


# ============================================================================
# 11. MISSING CATEGORY
# ============================================================================

test_that("risk_distinct_category rejects missing categories", {

  x <- make_distinct_data()
  x$category[1] <- NA

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "category_col"
  )
})


# ============================================================================
# 12. MISSING OBSERVED COUNT
# ============================================================================

test_that("risk_distinct_category rejects missing observed counts", {

  x <- make_distinct_data()
  x$event_count[1] <- NA

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "observed_col"
  )
})


# ============================================================================
# 13. NEGATIVE OBSERVED COUNT
# ============================================================================

test_that("risk_distinct_category rejects negative observed counts", {

  x <- make_distinct_data()
  x$event_count[1] <- -1

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "negative"
  )
})


# ============================================================================
# 14. FRACTIONAL OBSERVED COUNT
# ============================================================================

test_that("risk_distinct_category rejects fractional observed counts", {

  x <- make_distinct_data()
  x$event_count[1] <- 1.5

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "whole-number"
  )
})


# ============================================================================
# 15. INFINITE OBSERVED COUNT
# ============================================================================

test_that("risk_distinct_category rejects infinite observed counts", {

  x <- make_distinct_data()
  x$event_count[1] <- Inf

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "finite"
  )
})


# ============================================================================
# 16. MISSING DENOMINATOR
# ============================================================================

test_that("risk_distinct_category rejects missing denominators", {

  x <- make_distinct_data()
  x$exposure[1] <- NA

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "denominator_col"
  )
})


# ============================================================================
# 17. NEGATIVE DENOMINATOR
# ============================================================================

test_that("risk_distinct_category rejects negative denominators", {

  x <- make_distinct_data()
  x$exposure[1] <- -1

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "negative"
  )
})


# ============================================================================
# 18. INFINITE DENOMINATOR
# ============================================================================

test_that("risk_distinct_category rejects infinite denominators", {

  x <- make_distinct_data()
  x$exposure[1] <- Inf

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "finite"
  )
})


# ============================================================================
# 19. ZERO DENOMINATOR CATEGORY
# ============================================================================

test_that("zero-denominator categories are ineligible rather than crashing", {

  x <- data.frame(
    unit_id = c(
      "A",
      "B"
    ),
    category = c(
      "Water",
      "Water"
    ),
    event_count = c(
      0L,
      0L
    ),
    exposure = c(
      0,
      0
    )
  )

  out <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_true(
    all(
      out$insufficient_count_flag
    )
  )
})


# ============================================================================
# 20. INVALID MIN COUNT
# ============================================================================

test_that("risk_distinct_category rejects invalid minimum count", {

  x <- make_distinct_data()

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure",
      min_count = -1
    ),
    "non-negative whole"
  )

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure",
      min_count = 1.5
    ),
    "non-negative whole"
  )

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure",
      min_count = Inf
    ),
    "non-negative whole"
  )
})


# ============================================================================
# 21. INVALID include_lowest
# ============================================================================

test_that("risk_distinct_category validates include_lowest", {

  expect_error(
    risk_distinct_category(
      data = make_distinct_data(),
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure",
      include_lowest = NA
    ),
    "TRUE or FALSE"
  )
})


# ============================================================================
# 22. ZERO-ROW INPUT
# ============================================================================

test_that("risk_distinct_category supports zero-row input", {

  x <- make_distinct_data()[0, ]

  out <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_equal(
    nrow(out),
    0
  )

  expect_true(
    all(
      c(
        "highest_category",
        "highest_smr",
        "highest_event_count",
        "lowest_category",
        "lowest_smr",
        "lowest_event_count",
        "category_count_used",
        "insufficient_count_flag"
      ) %in%
        names(out)
    )
  )
})


# ============================================================================
# 23. sf INPUT
# ============================================================================

test_that("risk_distinct_category accepts sf and returns non-spatial summary", {

  x <- make_distinct_data()

  x$x_coord <- seq_len(nrow(x))
  x$y_coord <- rep(1, nrow(x))

  sf_x <- sf::st_as_sf(
    x,
    coords = c(
      "x_coord",
      "y_coord"
    ),
    crs = 3857
  )

  out <- risk_distinct_category(
    data = sf_x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_false(
    inherits(
      out,
      "sf"
    )
  )

  expect_equal(
    nrow(out),
    3
  )
})


# ============================================================================
# 24. DETERMINISTIC HIGHEST TIE BREAK
# ============================================================================

test_that("highest-category ties use count then category label", {

  x <- data.frame(
    unit_id = c(
      "A",
      "A",
      "B",
      "B"
    ),
    category = c(
      "Alpha",
      "Beta",
      "Alpha",
      "Beta"
    ),
    event_count = c(
      5L,
      5L,
      5L,
      5L
    ),
    exposure = c(
      100,
      100,
      100,
      100
    )
  )

  out <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_equal(
    out$highest_category,
    c(
      "Alpha",
      "Alpha"
    )
  )
})


# ============================================================================
# 25. UNIT ORDER PRESERVED
# ============================================================================

test_that("risk_distinct_category preserves first-seen unit order", {

  x <- make_distinct_data()

  x$unit_id <- factor(
    x$unit_id,
    levels = c(
      "C",
      "B",
      "A"
    )
  )

  x <- x[
    order(x$unit_id),
    ,
    drop = FALSE
  ]

  out <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure"
  )

  expect_equal(
    as.character(out$unit_id),
    c(
      "C",
      "B",
      "A"
    )
  )
})


# ============================================================================
# 26. OUTPUT NAMES MUST BE UNIQUE
# ============================================================================

test_that("risk_distinct_category rejects duplicate output names", {

  expect_error(
    risk_distinct_category(
      data = make_distinct_data(),
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure",
      highest_category_col = "x",
      highest_smr_col = "x"
    ),
    "must be unique"
  )
})


# ============================================================================
# 27. MISSING REQUIRED COLUMN
# ============================================================================

test_that("risk_distinct_category rejects missing required columns", {

  x <- make_distinct_data()
  x$exposure <- NULL

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "Missing required"
  )
})


# ============================================================================
# 28. NON-NUMERIC OBSERVED
# ============================================================================

test_that("risk_distinct_category rejects non-numeric observed counts", {

  x <- make_distinct_data()
  x$event_count <- as.character(x$event_count)

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "must be numeric"
  )
})


# ============================================================================
# 29. NON-NUMERIC DENOMINATOR
# ============================================================================

test_that("risk_distinct_category rejects non-numeric denominators", {

  x <- make_distinct_data()
  x$exposure <- as.character(x$exposure)

  expect_error(
    risk_distinct_category(
      data = x,
      unit_id_col = "unit_id",
      category_col = "category",
      denominator_col = "exposure"
    ),
    "must be numeric"
  )
})


# ============================================================================
# 30. MIN COUNT ZERO ALLOWS ZERO-COUNT CATEGORY ONLY IF SMR DEFINED
# ============================================================================

test_that("min_count zero still requires a defined comparative SMR", {

  x <- data.frame(
    unit_id = c(
      "A",
      "B"
    ),
    category = c(
      "Falls",
      "Falls"
    ),
    event_count = c(
      0L,
      10L
    ),
    exposure = c(
      100,
      100
    )
  )

  out <- risk_distinct_category(
    data = x,
    unit_id_col = "unit_id",
    category_col = "category",
    denominator_col = "exposure",
    min_count = 0
  )

  expect_equal(
    out$highest_smr[
      out$unit_id == "A"
    ],
    0
  )

  expect_false(
    out$insufficient_count_flag[
      out$unit_id == "A"
    ]
  )
})
