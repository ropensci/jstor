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


# definitions -----
# nolint start
standard_references <- structure(
  list(
    file_name = c("references", "references", "references"),
    ref_title = c("Bibliography: Entamoeba ranarumn", "Bibliography: Entamoeba ranarumn", 
                 "References: Leptotheca ohilmacheri"), 
    authors = c(NA_character_, NA_character_, NA_character_),
    collab = c(NA_character_, NA_character_, NA_character_), 
    title = c(NA_character_, NA_character_, NA_character_),
    year = c(NA_character_, NA_character_, NA_character_), 
    source = c(NA_character_, NA_character_, NA_character_),
    volume = c(NA_character_, NA_character_, NA_character_),
    first_page = c(NA_character_, NA_character_, NA_character_), 
    last_page = c(NA_character_, NA_character_, NA_character_),
    publisher = c(NA_character_, NA_character_, NA_character_), 
    publication_type = c(NA_character_, NA_character_, NA_character_), 
    unparsed_refs = c("DOBELL, C.C.\n1909 Researches on the intestinal Protozoa of frogs and toads. Quart. Jour. Micros.\nSc., 53:201-276, 4 pl. and 1 textfig.", 
                      "1918 Are Entamoeba histolytica and Entamoeba ranarum the same species? An experi-\nmental study. Parasit., 10:294-310.", 
                      "KUDO, R.\n1920 Studies on Myxosporidia. A Synopsis of Genera and Species of Myxosporidia.\nill. Biol. Monogr., 5:243-503, 25 pl. and 2 textfig.")),
  class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -3L))


no_references <- tibble::data_frame(
  file_name = "author-prefix",
  ref_title = NA_character_,
  authors = NA_character_,
  editors = NA_character_,
  collab = NA_character_,
  title = NA_character_,
  year = NA_character_,
  source = NA_character_,
  volume = NA_character_,
  first_page = NA_character_,
  last_page = NA_character_,
  publisher = NA_character_,
  publication_type = NA_character_,
  unparsed_refs = NA_character_
)


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


unparsed_refs <- c(
  NA_character_,
  "Producer Dynamics: New Evidence from Micro Data",
  "American Economic Review",
  NA_character_
)


parsed_refs <-  c("The USA PATRIOT Act expanded the government's surveillance power in numerous other ways (see, e.g. Keenan 2005 ).", 
                  "Acohido, B. and Eisler, P. ( 2013 ) “Snowden Case: How Low-Level Insider Could Steal from NSA” , USA Today , 12 June. Available online at http://www.google.com (accessed 15 June 2013).", 
                  "Amnesty International ( 2013 ) “USA: Revelations about Government Surveillance ‘raise red flags’” , 7 June. Available online at http://www.google.com (accessed 14 June 2013).", 
                  "Jacobson, D. , 2009 . Chapter title . In: D. E. Davis & J. Go , eds. Book title .: Routledge , pp. 281 - 286 .", 
                  "Costall, Alan ( 1980 ). “Some article title” Theory and Psychology 1 : 123 – 145 .", 
                  "Hudson, W. , 2000 . Another article title . Australian Journal of Cats & Dogs , September , 40 ( 3 ), p. 134 – 150 .", 
                  "Fries-Britt S. , & Griffin K.A. ( 2000 ). Some article about race . Journal of College Student Fun , 20 , 60 – 120 .")


unparsed_citations <- c(
  paste("Becker, Howard. 2010 [1982]. Les mondes de l’art. Paris, Flammarion",
        "(éd. orig. Art Worlds. Berkeley, The University of Califoria Press).")
)


correct_parsed <- structure(
  list(
    file_name = c("references-parsed", "references-parsed", "references-parsed",
                  "references-parsed", "references-parsed", "references-parsed", 
                  "references-parsed"), 
    ref_title = c("Notes", "References", "References", "References", 
                  "References", "References", "References"), 
    authors = c(NA, "Acohido, B.; Eisler, P.", NA, "Jacobson, D.",
                "Costall, Alan", "Hudson, W.", "Fries-Britt, S.; Griffin, K.A."),
    editors = c(NA, NA, NA, "D. E. Davis; J. Go", NA, NA, NA), 
    collab = c(NA, NA, "Amnesty International", NA, NA, NA, NA),
    title = c(NA, "“Snowden Case: How Low-Level Insider Could Steal from NSA”", 
              NA, "Chapter title", "“Some article title”", "Another article title", 
              "Some article about race"), 
    year = c("2005", "2013", "2013", "2009", "1980", "2000", "2000"), 
    source = c(NA, "USA Today", 
               "“USA: Revelations about Government Surveillance ‘raise red flags’”", 
               "Book title", "Theory and Psychology", "Australian Journal of Cats & Dogs", 
               "Journal of College Student Fun"), 
    volume = c(NA, NA, NA, NA, "1", "40", "20"), 
    first_page = c(NA, NA, NA, "281", "123", "134", "60"), 
    last_page = c(NA, NA, NA, "286", "145", "150", "120"), 
    publisher = c(NA, NA, NA, "Routledge", NA, NA, NA), 
    publication_type = c("other", "other", "other", "book", 
                         "journal", "journal", "journal"), 
    unparsed_refs = c("1. The USA PATRIOT Act expanded the government's surveillance power in numerous other ways (see, e.g. Keenan 2005 ).", 
                      "Acohido, B. and Eisler, P. ( 2013 ) “Snowden Case: How Low-Level Insider Could Steal from NSA” , USA Today , 12 June. Available online at http://www.google.com (accessed 15 June 2013).", 
                      "Amnesty International ( 2013 ) “USA: Revelations about Government Surveillance ‘raise red flags’” , 7 June. Available online at http://www.google.com (accessed 14 June 2013).", 
                      "Jacobson, D. , 2009 . Chapter title . In: D. E. Davis & J. Go , eds. Book title .: Routledge , pp. 281 - 286 .", 
                      "Costall, Alan ( 1980 ). “Some article title” Theory and Psychology 1 : 123 – 145 .", 
                      "Hudson, W. , 2000 . Another article title . Australian Journal of Cats & Dogs , September , 40 ( 3 ), p. 134 – 150 .", 
                      "Fries-Britt S. , & Griffin K.A. ( 2000 ). Some article about race . Journal of College Student Fun , 20 , 60 – 120 .")),
  class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -7L))

# nolint end
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
  expect_identical(jst_get_references("testfiles/author-prefix.xml"),
                   no_references)
})

test_that("standard references work", {
  expect_identical(result, standard_references)
})

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

test_that("extracting only the title works", {
  expect_equal(result_empty[["ref_title"]], NA_character_)
  expect_equal(result[["ref_title"]],
               c("Bibliography: Entamoeba ranarumn",
                 "Bibliography: Entamoeba ranarumn",
                 "References: Leptotheca ohilmacheri"))
})


test_that("parsing references works", {
  expect_identical(correct_parsed, jst_get_references(parsed, parse_refs = T))
})
