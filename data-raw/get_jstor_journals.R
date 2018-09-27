# this script details how to get the most recent data from jstor
library(jstor)

# if jst_get_journal_overview works correctly, the data can be updated via
jstor_journals <- jst_get_journal_overview(most_recent = T)
usethis::use_data(jstor_journals, internal = T, overwrite = T)

# update date in documentation
