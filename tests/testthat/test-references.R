context("extraction of references")

library(xml2)
library(magrittr)

# import files -----
result <- "testfiles/references.xml" %>%
  find_references()

result_empty <- "testfiles/empty_file.xml" %>%
  find_references()
 
# weird_pages <- "testfiles/test-file-weird-pages.xml"

unparsed <- "testfiles/unparsed-references.xml"

# single_and_multiple_references <- "testfiles/test-file-multiple-and-single-references.xml" %>%
#   find_references()
# 
# random_problem <- "testfiles/test-file-random-problem-reference.xml"
# 
# new_format <- "testfiles/test-file-new-format-nov-17.xml" %>%
#   find_references()


# tests -----
test_that("Input data is checked", {
  expect_error(find_references("my_path.txt"))
  expect_error(find_references("testfiles/standard_book.xml"), "You are using")
})

test_that("class is correct", {
  expect_s3_class(result, "jstor")
})

test_that("catching errors works", {
  expect_silent(find_references(unparsed))
  # expect_silent(find_references(weird_pages))
  # expect_silent(find_references(random_problem))
})

test_that("null results work", {
  expect_identical(result_empty[["full_reference"]], NA_character_)
  expect_identical(find_references(unparsed)[["full_reference"]][2],
                   NA_character_)
})


correct_refs <- c(
  "Bibliography: Entamoeba ranarumn",
  "DOBELL, C.C.
1909 Researches on the intestinal Protozoa of frogs and toads. Quart. Jour. Micros.
Sc., 53:201-276, 4 pl. and 1 textfig.",
  "1918 Are Entamoeba histolytica and Entamoeba ranarum the same species? An experi-
mental study. Parasit., 10:294-310.",
  "References: Leptotheca ohilmacheri",
  "KUDO, R.
1920 Studies on Myxosporidia. A Synopsis of Genera and Species of Myxosporidia.
ill. Biol. Monogr., 5:243-503, 25 pl. and 2 textfig."
)

unparsed_refs <- c(
  "References",
  NA_character_,
  "Producer Dynamics: New Evidence from Micro Data",
  "American Economic Review",
  NA_character_
)


test_that("extracting references works", {
  skip_on_os("windows")
  expect_identical(result[["full_reference"]], correct_refs)
  expect_identical(find_references(unparsed)[["full_reference"]], unparsed_refs)
  # expect_known_output(new_format[["full_reference"]], "testfiles/correct_new_format_refs.txt", print = T)
})


