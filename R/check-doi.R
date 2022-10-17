#' Check if a DOI exists
#'
#' @description 
#' Checks if a DOI (Digital Object Identifier) has a good structure, i.e.
#' `10.----/suffix`. Also checks if the redirection using the **DOI System**
#' (\url{https://www.doi.org/}) works. An internet connection is required.
#' 
#' @param doi a `character` of length 1. A single DOI to check.
#'
#' @return A `logical`. If the DOI has the good structure and exists on 
#' <https://www.doi.org/>, the returned value will be `TRUE`.
#' 
#' @export
#' 
#' @examples 
#' check_doi("bad_doi")
#' check_doi("10.1016/j.jviromet.2032.01.007")
#' check_doi("10.1016/j.jviromet.2008.01.007")

check_doi <- function(doi) {
  
  is_character(doi)
  
  doi <- gsub("\\s", "", doi)
  
  if (is_valid_doi(doi)) {
    
    response <- curl::curl_fetch_memory(paste0(doi_url(), doi))
    return(is_status_ok(response))
    
  } else {
    return(FALSE)
  }
}
