# =============================================================================
# TESTS — risk_export_workbook()
# =============================================================================


test_that("risk_export_workbook creates a valid workbook", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      area = c("North", "South"),
      Low = c(3, 4),
      High = c(1, 0),
      Total = c(4, 4)
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  out <- risk_export_workbook(
    tables = tables,
    path = path
  )

  expect_true(
    file.exists(path)
  )

  expect_gt(
    file.info(path)$size,
    0
  )

  expect_type(
    out,
    "character"
  )

  expect_length(
    out,
    1L
  )
})


test_that("risk_export_workbook returns path invisibly", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      area = "North",
      Total = 1
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_invisible(
    risk_export_workbook(
      tables = tables,
      path = path
    )
  )
})


test_that("risk_export_workbook creates expected sheets", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      area = c("North", "South"),
      Total = c(2, 3)
    ),

    Poisson = data.frame(
      area = c("North", "South"),
      probability = c(
        "P = 0.25\nn = 2 | 0.7/year",
        "P = 0.50\nn = 3 | 1.0/year"
      ),
      Total = c(
        "n = 2 | 0.7/year",
        "n = 3 | 1.0/year"
      )
    )
  )

  data_dictionary <- data.frame(
    field = c(
      "area",
      "Total"
    ),
    definition = c(
      "Synthetic area.",
      "Observed event count."
    )
  )

  methods <- data.frame(
    method = "Cross-tabulation",
    description = "Synthetic test method."
  )

  qa <- data.frame(
    check = "Row count",
    observed = "2",
    expected = "2",
    status = "PASS"
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  risk_export_workbook(
    tables = tables,
    path = path,
    data_dictionary = data_dictionary,
    methods = methods,
    qa = qa
  )

  wb <- openxlsx2::wb_load(
    path
  )

  sheets <- unname(
    openxlsx2::wb_get_sheet_names(
      wb
    )
  )

  expect_identical(
    sheets,
    c(
      "01_Counts",
      "02_Poisson",
      "03_Data_Dictionary",
      "04_Methods",
      "05_QA"
    )
  )
})


test_that("risk_export_workbook includes summary when context is supplied", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      area = "North",
      Total = 1
    )
  )

  context <- list(
    overview = list(
      title = "Synthetic Risk Analysis",
      purpose = "Test workbook export.",
      objective = "Validate summary generation.",
      dataset_represents = "Synthetic test records."
    ),

    scope = list(
      analysis_start = as.Date("2025-07-01"),
      analysis_end = as.Date("2026-06-30"),
      geographic_scope = "Synthetic areas",
      unit_of_analysis = "Event",
      inclusions = "Synthetic events",
      exclusions = "None"
    ),

    filters = data.frame(
      field = "status",
      rule = "Finalised"
    ),

    interpretation = list(
      notes = "Synthetic test output only."
    ),

    provenance = list(
      source = "Synthetic test dataset",
      owner = "Test suite"
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  risk_export_workbook(
    tables = tables,
    path = path,
    title = "Synthetic Risk Analysis",
    description = "Workbook export test.",
    context = context
  )

  wb <- openxlsx2::wb_load(
    path
  )

  sheets <- unname(
    openxlsx2::wb_get_sheet_names(
      wb
    )
  )

  expect_identical(
    sheets,
    c(
      "00_Summary",
      "01_Counts"
    )
  )
})


test_that("risk_export_workbook supports grouped tables", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Grouped_Counts = data.frame(
      Operation = c(
        "Recreation",
        "Recreation",
        "Infrastructure",
        "Infrastructure"
      ),
      Area = c(
        "North",
        "South",
        "North",
        "South"
      ),
      Total = c(
        2,
        3,
        1,
        4
      )
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_silent(
    risk_export_workbook(
      tables = tables,
      path = path,
      group_cols = c(
        Grouped_Counts = "Operation"
      )
    )
  )

  expect_true(
    file.exists(path)
  )
})


test_that("risk_export_workbook supports multiline cells", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Poisson = data.frame(
      Area = c(
        "North",
        "South"
      ),
      Critical = c(
        "P = 0.323\nn = 3 | 1.5/year",
        "0 events"
      ),
      Total = c(
        "n = 3 | 1.5/year",
        "n = 0 | 0.0/year"
      )
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_silent(
    risk_export_workbook(
      tables = tables,
      path = path
    )
  )

  expect_true(
    file.exists(path)
  )
})


test_that("risk_export_workbook respects overwrite", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      area = "North",
      Total = 1
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  risk_export_workbook(
    tables = tables,
    path = path
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = path
    ),
    "already exists"
  )

  expect_silent(
    risk_export_workbook(
      tables = tables,
      path = path,
      overwrite = TRUE
    )
  )
})


test_that("risk_export_workbook rejects invalid tables input", {

  skip_if_not_installed("openxlsx2")

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_error(
    risk_export_workbook(
      tables = data.frame(
        x = 1
      ),
      path = path
    ),
    "non-empty named list"
  )

  expect_error(
    risk_export_workbook(
      tables = list(
        data.frame(
          x = 1
        )
      ),
      path = path
    ),
    "unique, non-empty names"
  )

  expect_error(
    risk_export_workbook(
      tables = list(
        Counts = 1:3
      ),
      path = path
    ),
    "data frame or tibble"
  )
})


test_that("risk_export_workbook rejects invalid paths", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      x = 1
    )
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = ""
    ),
    "single non-empty character"
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = tempfile(
        fileext = ".csv"
      )
    ),
    "\\.xlsx"
  )
})


test_that("risk_export_workbook validates optional data frames", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      x = 1
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = path,
      data_dictionary = "invalid"
    ),
    "data_dictionary must be NULL or a data frame"
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = path,
      methods = 1
    ),
    "methods must be NULL or a data frame"
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = path,
      qa = list()
    ),
    "qa must be NULL or a data frame"
  )
})


test_that("risk_export_workbook validates group_cols", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Grouped_Counts = data.frame(
      Operation = c(
        "Recreation",
        "Infrastructure"
      ),
      Total = c(
        2,
        3
      )
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = path,
      group_cols = "Operation"
    ),
    "named character vector"
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = path,
      group_cols = c(
        Unknown_Table = "Operation"
      )
    ),
    "match names in tables"
  )

  expect_error(
    risk_export_workbook(
      tables = tables,
      path = path,
      group_cols = c(
        Grouped_Counts = "NotAColumn"
      )
    ),
    "was not found"
  )
})


test_that("risk_export_workbook handles empty analytical tables", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Empty = data.frame(
      area = character(),
      Total = integer()
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_silent(
    risk_export_workbook(
      tables = tables,
      path = path
    )
  )

  expect_true(
    file.exists(path)
  )
})


test_that("risk_export_workbook is silent during normal export", {

  skip_if_not_installed("openxlsx2")

  tables <- list(
    Counts = data.frame(
      area = "North",
      Total = 1
    )
  )

  path <- tempfile(
    fileext = ".xlsx"
  )

  expect_silent(
    risk_export_workbook(
      tables = tables,
      path = path
    )
  )
})