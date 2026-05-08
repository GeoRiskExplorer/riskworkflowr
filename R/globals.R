# 1 ------------------------------------------------------------------------
# globals.R
# Purpose: Declare non-standard evaluation globals for R CMD check

utils::globalVariables(
  c(
    ".data",
    ":=",
    "all_of",
    "join_method",
    ".observed_tmp",
    ".expected_tmp",
    ".lower_observed_tmp",
    ".upper_observed_tmp"
  )
)