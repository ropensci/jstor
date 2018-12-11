context("writing-to-file")
library(readr)

paths <- c("testfiles/standard_case.xml", "broken_path.txt")

# # nolint start
# # prepare correct result -----
# "tests/testthat/testfiles/standard_case.xml" %>%
#   jst_get_article() %>%
#   write_csv("tests/testthat/testfiles/correct_meta_data.csv", col_names = T)
# 
# "tests/testthat/testfiles/standard_case.xml" %>%
#   jst_get_article() %>%
#   write_csv("tests/testthat/testfiles/correct_meta_data_wo_cols.csv",
#             col_names = FALSE)
# # nolint end

# tests ------
test_that("writing correct results to file works", {
  temp_dir <- tempdir()
  jstor_convert_to_file(paths, 1, file.path(temp_dir, "/meta_data"), 
                        jst_get_article, col_names = TRUE, n_batches = 1)

  expect_identical(read_csv("testfiles/correct_meta_data.csv",
                            col_names = TRUE),
                   read_csv(file.path(temp_dir, "/meta_data-1.csv"),
                            col_names = TRUE))

  unlink(temp_dir)
})

test_that("writing to file in parallel works", {
  future::plan(future::multiprocess)
  temp_dir <- tempdir()
  jstor_convert_to_file(paths, 1, file.path(temp_dir, "/meta_data"), jst_get_article,
                        col_names = TRUE, n_batches = 1)
  
  expect_identical(read_csv("testfiles/correct_meta_data.csv",
                            col_names = TRUE),
                   read_csv(file.path(temp_dir, "/meta_data-1.csv"),
                            col_names = TRUE))
  
  unlink(temp_dir)
})

test_that("not writing column names works", {
  temp_dir <- tempdir()
  jstor_convert_to_file(paths, 1, file.path(temp_dir, "/meta_data"), jst_get_article,
                        col_names = FALSE, n_batches = 1)
  
  expect_identical(read_csv("testfiles/correct_meta_data_wo_cols.csv",
                            col_names = FALSE),
                   read_csv(file.path(temp_dir, "/meta_data-1.csv"),
                            col_names = FALSE))
  
  unlink(temp_dir)
})


test_that("writing error messages to file works", {
  temp_dir <- tempdir()

  jstor_convert_to_file(paths, 1, file.path(temp_dir, "meta_data"), jst_get_article,
                        n_batches = 1)

  res <- read_csv(file.path(temp_dir, "meta_data_broken-1.csv"),
                  col_names = TRUE, col_types = cols(id = col_integer()))

  # the following is needed for expect_identical
  attr(res, "spec") <- NULL

  correct_res <- structure(
    list(
      id = 2L, 
      error_message = "Unknown input file. Must be a `xml`-file."
    ), 
    row.names = c(NA, -1L), 
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))

  expect_identical(res, correct_res)

  unlink(temp_dir)
})


test_that("import wrapper works with column names", {
  temp_dir <- tempdir()
  jst_import(paths, out_file = "meta_data", out_path = temp_dir,
             .f = jst_get_article, col_names = T)

  expect_identical(read_csv("testfiles/correct_meta_data.csv",
                            col_names = TRUE),
                   read_csv(file.path(temp_dir, "/meta_data-1.csv"),
                            col_names = TRUE))

  unlink(temp_dir)
})

test_that("import wrapper works without column names", {
  temp_dir <- tempdir()
  jst_import(paths, out_file = "meta_data", out_path = temp_dir,
             .f = jst_get_article, col_names = FALSE)
  
  expect_identical(read_csv("testfiles/correct_meta_data_wo_cols.csv",
                            col_names = FALSE),
                   read_csv(file.path(temp_dir, "/meta_data-1.csv"),
                            col_names = FALSE))
  
  unlink(temp_dir)
})

test_that("files_per_batch works", {
  temp_dir <- tempdir()
  jst_import(paths, out_file = "meta_data", out_path = temp_dir,
             .f = jst_get_article, col_names = FALSE, files_per_batch = 2)
  
  expect_identical(read_csv("testfiles/correct_meta_data_wo_cols.csv",
                            col_names = FALSE),
                   read_csv(file.path(temp_dir, "/meta_data-1.csv"),
                            col_names = FALSE))
  
  unlink(temp_dir)
})

test_that("n_batches works for n > 1", {
  temp_dir <- tempdir()
  
  paths <- c(paths, paths[1])
  
  jst_import(paths, out_file = "meta_data", out_path = temp_dir,
             .f = jst_get_article, col_names = FALSE, n_batches = 2)
  
  written_files <- list.files(temp_dir, full.names = T, pattern = "meta_data-")
  
  expect_identical(length(written_files), 2L)
  
  expect_equal(
    # duplicate correct result, since we read it in duplicated above
    read_csv("testfiles/correct_meta_data_wo_cols.csv", col_names = FALSE) %>% 
      dplyr::slice(c(1, 1)),
    purrr::map_df(written_files, read_csv, col_names = FALSE)
  )
                   
  unlink(temp_dir)
})

test_that("too many arguments for batches throw error", {
  expect_error(jst_import(paths, out_file = "meta_data",
                          out_path = temp_dir, .f = jst_get_article,
                          n_batches = 2, files_per_batch = 1),
               "Either n_batches"
  )
  
  expect_error(jst_import_zip("testfiles/pseudo_dfr.zip", out_file = "meta_data",
                            import_spec = jst_define_import(
                              article = jst_get_article
                            ),
                            n_batches = 2, files_per_batch = 1),
               "Either n_batches"
  )
})

