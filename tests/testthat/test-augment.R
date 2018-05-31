context("test-augment.R")
library(dplyr)
library(tibble)


test_that("converting pages works", {
  in_vec <- c("33", "M66", "67M", "AJSv104p126", NA_character_)
  out_vec <- c(33L, 66L, 67L, 126L, NA_integer_)
  
  expect_identical(jst_clean_page(in_vec), out_vec)
  
})

test_that("total pages are computed", {
  input <- tribble(
    ~first_page,   ~last_page,   ~page_range,
    NA_character_, NA_character_, NA_character_,
    1,             10,            "1 - 10",
    1,             10,            NA_character_,
    1,             NA_character_, NA_character_,
    1,             NA_character_, "1-10"
  )
  
  correct_output <- c(NA_real_, 10, 10, NA_real_, 10)

  out <- input %>% 
    mutate(out = jst_get_total_pages(first_page, last_page, page_range)) %>% 
    pull(out)
  
  expect_identical(correct_output, out)
  expect_error(jst_get_total_pages(1, 1, 1:2)) # unequal lengths raise error
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
