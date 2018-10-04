context("extraction of references")

library(xml2)
library(magrittr)

# import files -----
result <- "testfiles/references.xml" %>%
  jst_get_references()

result_empty <- "testfiles/empty_file.xml" %>%
  jst_get_references()

half_empty <- "testfiles/references-half-empty.xml"

unparsed <- "testfiles/unparsed-references.xml"

unparsed_citation <- "testfiles/unparsed-citation.xml"

parsed <- "testfiles/references-parsed.xml"

unknown <- "testfiles/unknown-reference.xml"

# tests -----
test_that("Input data is checked", {
  expect_error(jst_get_references("my_path.txt"))
  expect_error(jst_get_references("testfiles/standard_book.xml"), "You are using")
})

test_that("class is correct", {
  expect_s3_class(result, "tbl_df")
})

test_that("catching errors works", {
  expect_silent(jst_get_references(unparsed))
})

test_that("null results work", {
  expect_identical(result_empty[["unparsed_refs"]], NA_character_)
  expect_identical(jst_get_references(unparsed)[["unparsed_refs"]][1],
                   NA_character_)
  expect_identical(jst_get_references(half_empty)[["unparsed_refs"]],
                   NA_character_)
})

# nolint start
correct_refs <- c(
  "DOBELL, C.C.
1909 Researches on the intestinal Protozoa of frogs and toads. Quart. Jour. Micros.
Sc., 53:201-276, 4 pl. and 1 textfig.",
  "1918 Are Entamoeba histolytica and Entamoeba ranarum the same species? An experi-
mental study. Parasit., 10:294-310.",
  "KUDO, R.
1920 Studies on Myxosporidia. A Synopsis of Genera and Species of Myxosporidia.
ill. Biol. Monogr., 5:243-503, 25 pl. and 2 textfig."
)
# nolint end

unparsed_refs <- c(
  NA_character_,
  "Producer Dynamics: New Evidence from Micro Data",
  "American Economic Review",
  NA_character_
)

parsed_refs <- c(
  paste("The USA PATRIOT Act expanded the government's surveillance power in",
        "numerous other ways (see, e.g. Keenan 2005 )."),
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

unparsed_citations <- c(
  paste("Becker, Howard. 2010 [1982]. Les mondes de l’art. Paris, Flammarion",
        "(éd. orig. Art Worlds. Berkeley, The University of Califoria Press).")
)


test_that("extracting references works", {
  skip_on_os("windows")
  expect_identical(result[["unparsed_refs"]], correct_refs)
  expect_identical(jst_get_references(unparsed)[["unparsed_refs"]], unparsed_refs)
  expect_identical(jst_get_references(unparsed_citation)[["unparsed_refs"]],
                   unparsed_citations)
  expect_identical(jst_get_references(parsed)[["unparsed_refs"]], parsed_refs)
  expect_error(jst_get_references(unknown),
               paste("Unknown citation format in file",
                     "`testfiles/unknown-reference.xml`"))
})

# nolint start
correct_parsed <- structure(
  list(
    file_name = c("references-parsed", "references-parsed",  "references-parsed"), 
    ref_title = c("Notes", "References", "References" ),
    authors = c(NA, "Acohido, B.; Eisler, P.", NA), 
    collab = c(NA, NA, "Amnesty International"), 
    title = c(NA, "“Snowden Case: How Low-Level Insider Could Steal from NSA”", 
              NA), 
    year = c("2005", "2013", "2013"), 
    source = c(NA, "USA Today", 
               "“USA: Revelations about Government Surveillance ‘raise red flags’”"),
    unparsed_refs = c("1. The USA PATRIOT Act expanded the government's surveillance power in numerous other ways (see, e.g. Keenan 2005 ).", 
                      "Acohido, B. and Eisler, P. ( 2013 ) “Snowden Case: How Low-Level Insider Could Steal from NSA” , USA Today , 12 June. Available online at http://www.usatoday.com/story/news/nation/2013/06/11/snowden-nsa-hacking-privileged-accounts/2412507/ (accessed 15 June 2013).", 
                      "Amnesty International ( 2013 ) “USA: Revelations about Government Surveillance ‘raise red flags’” , 7 June. Available online at http://www.amnesty.org/en/news/usa-revelations-about-government-surveillance-raise-red-flags-2013–06–07 (accessed 14 June 2013).")), 
  class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -3L))
# nolint end

test_that("parsing references works", {
  expect_identical(correct_parsed, jst_get_references(parsed, parse_refs = T))
})
