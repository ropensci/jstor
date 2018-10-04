context("test-augment.R")


test_that("converting pages works", {
  in_vec <- c("33", "M66", "67M", "AJSv104p126", NA_character_)
  out_vec <- c(33L, 66L, 67L, 126L, NA_integer_)
  
  expect_identical(jst_clean_page(in_vec), out_vec)
  
})

test_that("total pages are computed", {
  input <- tibble::tribble(
    ~first_page,   ~last_page,    ~page_range,
    NA_real_,      NA_real_,      NA_character_,
    1,             10,            "1 - 10",
    1,             10,            NA_character_,
    1,             NA_real_,      NA_character_,
    1,             NA_real_,      "1-10"
  )
  
  correct_output <- c(NA_real_, 10, 10, NA_real_, 10)

  out <- input %>% 
    dplyr::mutate(out = jst_get_total_pages(
      first_page, last_page, page_range
    )) %>% 
    dplyr::pull(out)
  
  out2 <- jst_add_total_pages(input) %>% dplyr::pull(n_pages)
  
  expect_identical(correct_output, out)
  expect_identical(correct_output, out2)
  expect_error(jst_get_total_pages(1, 1, c("A", "B"))) # unequal lengths raise error
  expect_error(jst_get_total_pages("2", 2, "A"))
  expect_error(jst_get_total_pages(2, "2", "A"))
  expect_error(jst_get_total_pages(2, 2, 2))
})

test_that("total pages are added", {
  input <- tibble::tribble(
    ~first_page, ~last_page, ~page_range,
    1,           10,         NA_character_
  )

  output <- jst_add_total_pages(input)

  expect_identical(dplyr::bind_cols(input, n_pages = 10), output)
})

test_that("ranges are parsed correctly", {
  
  expect_identical(parse_ranges("1, 5-10"), 7)
  expect_identical(parse_ranges("11, 15-20"), 7)
  expect_identical(parse_ranges("1-6, 10"), 7)
  expect_identical(parse_ranges("1-4, 8-10"), 7)
  expect_identical(parse_ranges("1-2+4-5+8-10"), 7)
  expect_identical(parse_ranges("1-2, C3"), 3)
  expect_identical(parse_ranges("1-2, C3-C4"), 4)
  expect_identical(parse_ranges("1-10"), 10)
  
  expect_warning(parse_ranges("xiv-xx"))
  expect_warning(parse_ranges("X-XX"))
})


test_that("journal id is unified", {
  meta_data <- jst_get_article(jst_example("article_with_references.xml"))
  
  output <- jst_unify_journal_id(meta_data)
  correct_out <- dplyr::select(meta_data,
                               -journal_doi, -journal_pub_id,
                               journal_id = journal_jcode)
  
  expect_equal(output, correct_out)
})

test_that("data gets augmented", {
  journal_meta <- jst_get_article(jst_example("article_with_references.xml"))

  correct_journal <- structure(
    list(
      file_name = "article_with_references", 
      journal_title = "Transactions of the American Microscopical Society", 
      article_doi = "10.2307/3221896", article_pub_id = NA_character_, 
      article_jcode = NA_character_, article_type = "research-article", 
      article_title = "On the Protozoa Parasitic in Frogs", volume = "41", 
      issue = "2", language = "eng", pub_day = "1", pub_month = "4",  
      pub_year = 1922L, first_page = 59L, last_page = 76L, page_range = "59-76",
      journal_id = "tranamermicrsoci", n_pages = 18),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -1L)
  )
  
  expect_identical(correct_journal, jst_augment(journal_meta))
  expect_error(jst_augment(data.frame(a = 1, b = 2)))
})
