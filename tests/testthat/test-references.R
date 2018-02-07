context("extraction of references")

library(xml2)
library(magrittr)

# import files -----
result <- "testfiles/references.xml" %>%
  find_references()

result_empty <- "testfiles/empty_file.xml" %>%
  find_references()

half_empty <- "testfiles/references-half-empty.xml"

unparsed <- "testfiles/unparsed-references.xml"

parsed <- "testfiles/references-parsed.xml"


# tests -----
test_that("Input data is checked", {
  expect_error(find_references("my_path.txt"))
  expect_error(find_references("testfiles/standard_book.xml"), "You are using")
})

test_that("class is correct", {
  expect_s3_class(result, "tbl_df")
})

test_that("catching errors works", {
  expect_silent(find_references(unparsed))
})

test_that("null results work", {
  expect_identical(result_empty[["references"]], NA_character_)
  expect_identical(find_references(unparsed)[["references"]][2],
                   NA_character_)
  expect_identical(find_references(half_empty)[["references"]],
                   NA_character_)
})

# nolint start
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
# nolint end

unparsed_refs <- c(
  "References",
  NA_character_,
  "Producer Dynamics: New Evidence from Micro Data",
  "American Economic Review",
  NA_character_
)

parsed_refs <- c(
  "Notes",
  paste("The USA PATRIOT Act expanded the government's surveillance power in",
        "numerous other ways (see, e.g. Keenan 2005 )."),
  "References",
  paste0("Acohido, B. and Eisler, P. ( 2013 ) “Snowden Case: How Low-Level ",
         "Insider Could Steal from NSA” , USA Today , 12 June. Available ",
         "online at http://www.usatoday.com/story/news/nation/2013/06/11/",
         "snowden-nsa-hacking-privileged-accounts/2412507/ ",
         "(accessed 15 June 2013)."),
  paste0("Amnesty International ( 2013 ) “USA: Revelations about Government ",
         "Surveillance ‘raise red flags’” , 7 June. Available online at ",
         "http://www.amnesty.org/en/news/usa-revelations-about-government-",
         "surveillance-raise-red-flags-2013–06–07 (accessed 14 June 2013).")
)


test_that("extracting references works", {
  skip_on_os("windows")
  expect_identical(result[["references"]], correct_refs)
  expect_identical(find_references(unparsed)[["references"]], unparsed_refs)
  expect_identical(find_references(parsed)[["references"]], parsed_refs)
})
