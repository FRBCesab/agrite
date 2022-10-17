#' Find the DOI of a citation using the Web of Science Lite and CrossRef APIs
#'
#' @description
#' This function is a wrapper around [`search_in_wos`] and [`search_in_cr`].
#' 
#' From a citation, extracts the title and searches the corresponding 
#' DOI on the Web of Science database using the WOS Lite API 
#' (\url{https://developer.clarivate.com/apis/woslite}) and the R package
#' `rwoslite` (\url{https://github.com/frbcesab/rwoslite}). If the WOS Lite API
#' does not return any match, then the search is performed using the CrossRef 
#' API 
#' (\url{https://www.crossref.org/documentation/retrieve-metadata/rest-api/}) 
#' and the R package `rcrossref`
#' (\url{https://cran.r-project.org/package=rcrossref}).
#' 
#' **Important**: for the WOS Lite API, a token must be first obtained and 
#' stored locally. See \url{https://github.com/frbcesab/rwoslite} for further 
#' information. For the CrossRef API, you need to share your email address with
#' the API. See
#' \url{https://docs.ropensci.org/rcrossref/#register-for-the-polite-pool} 
#' for further information.
#' 
#' The extraction of the title is performed by the function 
#' [`title_from_citation`] and may fail if the citation is malformed.
#' 
#' @param x a `character` of length 1. A citation written as
#'   "Authors Year Title. Journal...". See [`title_from_citation`] for further
#'   detail.
#'
#' @param limit an `integer` of length 1. The maximum number of records to test
#'   to identify the match. Must be strictly positive and < 100,0000. Only used
#'   with WOS Lite API.
#'
#' @return A `data.frame` with one row and the following 5 columns:
#'   - `reference`: the original citation;
#'   - `search_term`: the cleaned title extracted from the citation used as the
#'     query terms in APIs;
#'   - `best_title`: the title of the best reference returned by WOS/CrossRef;
#'   - `best_doi`: the DOI corresponding to the best reference returned by 
#'     WOS/CrossRef;
#'   - `string_dist`: the string distance (Optimal string aligment, `osa`)
#'     between the original title and the best match. 
#'     See [`stringdist::stringdist`] for further information.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' ref <- paste0("Quainoo, A.K., Wetten, A.C., Allainguillaume, J., 2008. The ",
#'               "effectiveness of somatic embryogenesis in eliminating the ", 
#'               "cocoa swollen shoot virus from infected co- coa trees. ", 
#'               "J. Virol. Methods 149, 91e96.")
#'               
#' search_doi(ref)
#' }

search_doi <- function(x, limit = 1) {
  
  ## Check arguments ----
  
  is_character(x)
  is_integer(limit)
  
  
  ## Find DOI in WOS ----
  
  data_wos <- search_in_wos(x, limit = limit)
  
  if (!is.na(data_wos$"wos_best_doi")) doi_source <- "wos" else doi_source <- NA
  
  
  ## Find DOI in CrossRef (if required) ----
  
  if (is.na(data_wos$"wos_best_doi")) {
    
    data_cr <- search_in_cr(x)
    
    if (!is.na(data_cr$"cr_best_doi")) doi_source <- "cr" else doi_source <- NA
    
  } else {
    
    data_cr <- data.frame("search_term"    = NA,
                          "cr_best_title"  = NA,
                          "cr_best_doi"    = NA,
                          "cr_string_dist" = NA)
  }
  
  results <- data.frame(data_wos, data_cr)
  
  
  ## Store results ----
  
  datas <- data.frame("reference"   = x, 
                      "search_term" = data_wos$"search_term",
                      "best_title"  = NA,
                      "best_doi"    = NA,
                      "string_dist" = NA,
                      "source"      = NA)
  
  if (!is.na(doi_source)) {
    
    datas$"best_title"  <- results[1, paste0(doi_source, "_best_title")]
    datas$"best_doi"    <- results[1, paste0(doi_source, "_best_doi")]
    datas$"string_dist" <- results[1, paste0(doi_source, "_string_dist")]
    datas$"source"      <- toupper(doi_source)
  }
  
  datas
}
