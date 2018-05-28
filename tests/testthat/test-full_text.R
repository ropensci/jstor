context("full_text")

# base path for id ------
crazy_path <- "../Entpackt//soc_a-j/jpalestud/2012/41/2/10.1525%2Fjps.2012.xli.issue-2/10.1525%2Fjps.2012.xli.2.204/10.1525%2Fjps.2012.xli.2.204.txt" # nolint
standard_path <- "../Entpackt//soc_k-z/sozialewelt/1952/3/4/i40039236/40874627/40874627.txt" # nolint

test_that("getting base name of file works", {
  expect_identical(jst_get_file_name(crazy_path),
                   "10.1525%2Fjps.2012.xli.2.204")
  expect_identical(jst_get_file_name(standard_path), "40874627")
})

# encoding -----
file_path <- "testfiles/full-text.txt"

test_that("guessing encoding works", {
  expect_identical(get_encoding(file_path), "ASCII")
})

# text ------
# nolint start
lorem <- '<plain_text> <page sequence="1">Lorem ipsum dolor sit amet, 
consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore 
magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in 
voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim
id est laborum.
</page>  </plain_text>\n'
# nolint end

test_that("full-text is read correctly", {
  text <- jst_get_full_text(file_path)

  expect_identical(lorem, text[["full_text"]])
})
