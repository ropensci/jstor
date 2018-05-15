#' Get table with information on journals
#' 
#' Download most recent or display cached version of data on journals.
#' 
#' When analysing your sample of articles from DfR, it might be helpful to have
#' some context about the journals in your sample. This function provides a
#' `tibble` with various information like the full name of the journal, the 
#' short version of the name (sometimes referred to as `JCODE`), dates on where
#' the first
#' and last (available) issues were published, etc.
#' 
#' The data on journals might change. Therefore this function provides two
#' sources of data: a cached version which gets updated with every release, and
#' the ability to pull the most recent version directly from DfR.
#' 
#' The cached version was updated on 2018-05-15.
#' 
#' @param most_recent Should the most recent version be downloaded from DfR?
#' 
#' @return A `tibble` with various information about journals.
#' 
#' @export
#' @examples 
#' # use the function without arguments to get a tibble from disk
#' jst_get_journal_overview()
#' 
#' \dontrun{
#' # download the most recent version from DfR
#' jst_get_journal_overview(most_recent = TRUE)
#' }
jst_get_journal_overview <- function(most_recent = FALSE) {
  if (most_recent) {
    link <- "https://www.jstor.org/kbart/collections/all-archive-titles?fileFormat=txt"
    
    message("Downloading files from ", link)
    
    # the file and therefore the import is currently quite broken
    journals <- suppressWarnings(
      readr::read_tsv(link, col_types = "cccDccDccccccccccDDcccccccccccDD")
    )
    
    # remove attributes
    attr(journals, "problems") <- NULL
    attr(journals, "spec") <- NULL
    
    # use this heuristic to get only proper journals and remove some columns
    journals %>% 
      dplyr::filter(!is.na(full_coverage)) %>% 
      dplyr::select(-dplyr::one_of("publication_date", "first_author",
                                   "first_editor"))
    
  } else {
    jstor_journals
  }
}
