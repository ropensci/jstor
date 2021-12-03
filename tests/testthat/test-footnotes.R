# import files -----
result_erratum <- "testfiles/erratum.xml" %>%
  jst_get_footnotes()

result <- "testfiles/footnotes.xml" %>%
  jst_get_footnotes()

single_and_multiple_footnotes <- "testfiles/footnotes_mangled.xml" %>%
 jst_get_footnotes()

footnotes_in_body <- "testfiles/footnotes-in-body.xml" %>%
  jst_get_footnotes()

# tests -----
test_that("Input data is checked", {
  expect_error(jst_get_footnotes("my_path.txt"))
  expect_error(jst_get_footnotes("testfiles/standard_book.xml"), 
               class = "article_function_for_book")
})

test_that("class is correct", {
  expect_s3_class(single_and_multiple_footnotes, "tbl_df")
})

test_that("extracting footnotes works", {
  expect_identical(result_erratum[["footnotes"]][[1]], NA_character_)
  expect_snapshot_output(print(result[["footnotes"]]))
  expect_snapshot_output(print(single_and_multiple_footnotes[["footnotes"]]))
  expect_identical(footnotes_in_body[[1, "footnotes"]],
                   paste("1Martin Carnoy, “The Costs and Returns to Education",
                         "in Mexico” (PhD diss., University of Chicago, 1964).")
  )
})
