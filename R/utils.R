release_questions <- function() {
 paste("Have you updated the journal database and the corresponding date in",
       "the documentation?")
}

tidyr_new_interface <- function() {
  packageVersion("tidyr") > "0.8.99"
}
