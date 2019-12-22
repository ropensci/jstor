# This script serves to precompile the vignettes to update them for 
# docs.ropensci.org

library(knitr)
knit("vignettes/analysing-n-grams.Rmd.orig", "vignettes/analysing-n-grams.Rmd")

fs::dir_copy("figure", "vignettes/figure", overwrite = TRUE)
fs::dir_delete("figure")
