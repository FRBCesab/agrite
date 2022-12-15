#' Retrieve article metadata from a DOI
#'
#' @description 
#' Retrieves publication metadata from a DOI by querying Web of Science Lite 
#' API. See [search_in_wos()] for further information.
#' 
#' @param doi a character of length 1. The Digital Object Identifier (DOI) of 
#'   the publication fo which metadata must be retrieved.
#'
#' @return A `data.frame` with publication metadata. If no match in WOS, the
#' `data.frame` will be empty.
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' get_metadata("10.1111/ele.13778")
#' }
get_metadata <- function(doi) {
  
  check_doi(doi)
  
  ## WOS query ----
  
  search_terms <- paste0("DO=(", doi, ")")
  
  tryCatch(
    rwoslite::wos_get_records(search_terms, limit = 1),
    error = function(e) data.frame())
}
