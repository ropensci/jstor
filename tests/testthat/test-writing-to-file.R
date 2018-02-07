library(readr)
context("writing-to-file")

paths <- c("testfiles/standard_case.xml", "broken_path.txt")

# # nolint start
# # prepare correct result -----
# "tests/testthat/testfiles/standard_case.xml" %>%
#   find_article() %>%
#   write_csv("tests/testthat/testfiles/correct_meta_data.csv", col_names = T)
# 
# "tests/testthat/testfiles/standard_case.xml" %>%
#   find_article() %>%
#   write_csv("tests/testthat/testfiles/correct_meta_data_wo_cols.csv",
#             col_names = FALSE)
# # nolint end

# tests ------
test_that("writing correct results to file works", {
  temp_dir <- tempdir()
  jstor_convert_to_file(paths, 1, paste0(temp_dir, "/meta_data"), find_article,
                        col_names = TRUE, n_batches = 1)

  expect_identical(read_csv("testfiles/correct_meta_data.csv",
                            col_names = TRUE),
                   read_csv(paste0(temp_dir, "/meta_data-1.csv"),
                            col_names = TRUE))

  unlink(temp_dir)
})

test_that("not writing column names works", {
  temp_dir <- tempdir()
  jstor_convert_to_file(paths, 1, paste0(temp_dir, "/meta_data"), find_article,
                        col_names = FALSE, n_batches = 1)
  
  expect_identical(read_csv("testfiles/correct_meta_data_wo_cols.csv",
                            col_names = FALSE),
                   read_csv(paste0(temp_dir, "/meta_data-1.csv"),
                            col_names = FALSE))
  
  unlink(temp_dir)
})


test_that("writing error messages to file works", {
  temp_dir <- tempdir()

  jstor_convert_to_file(paths, 1, paste0(temp_dir, "meta_data"), find_article,
                        n_batches = 1)

  res <- read_csv(paste0(temp_dir, "meta_data_broken-1.csv"),
                  col_names = TRUE)

  # the following is needed for expect_identical
  attr(res, "spec") <- NULL

  correct_res <- data_frame(
    id = 2L,
    error_message = "`file_path` must be a `*.xml` file"
  )

  expect_identical(res, correct_res)

  unlink(temp_dir)
})


test_that("import wrapper works with column names", {
  temp_dir <- tempdir()
  jstor_import(paths, out_file = "meta_data", out_path = temp_dir,
               .f = find_article, col_names = T)

  expect_identical(read_csv("testfiles/correct_meta_data.csv",
                            col_names = TRUE),
                   read_csv(paste0(temp_dir, "/meta_data-1.csv"),
                            col_names = TRUE))

  unlink(temp_dir)
})

test_that("import wrapper works without column names", {
  temp_dir <- tempdir()
  jstor_import(paths, out_file = "meta_data", out_path = temp_dir,
               .f = find_article, col_names = FALSE)
  
  expect_identical(read_csv("testfiles/correct_meta_data_wo_cols.csv",
                            col_names = FALSE),
                   read_csv(paste0(temp_dir, "/meta_data-1.csv"),
                            col_names = FALSE))
  
  unlink(temp_dir)
})
