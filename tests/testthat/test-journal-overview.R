context("test-journal-overview.R")

in_memory <- jst_get_journal_overview()

correct_names <- c(
  "title", "journal_id", "issn", "eissn", "doi", "url", "discipline", 
  "publisher", "coverage_range", "oclc_catalog_identifier", 
  "lccn_catalog_identifier", "archive_release_date"
)

test_that("in memory journal list is available", {
  expect_s3_class(in_memory, "tbl_df")
  expect_identical(names(in_memory), correct_names)
  expect_identical(nrow(in_memory), 4220L)
})

test_that("out of memory journal list is available and correct", {
  skip("Downloading was disabled")
  out_of_memory <- jst_get_journal_overview(most_recent = T, quiet = T)
  
  expect_s3_class(out_of_memory, "tbl_df")
  expect_identical(names(out_of_memory), correct_names)
  expect_gt(nrow(out_of_memory), 4100)
  expect_lt(nrow(out_of_memory), 4300)
})

test_that("Trying to download raises a warning", {
   expect_warning(jst_get_journal_overview(most_recent = T, quiet = T))
})


