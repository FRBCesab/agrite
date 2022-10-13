#' Find the DOI of a citation reference using the Web of Science Lite API
#'
#' @description
#' From a citation reference, extracts the title and searches the corresponding 
#' DOI on the Web of Science database using the WOS Lite API 
#' (\url{https://developer.clarivate.com/apis/woslite}) and the R package
#' `rwoslite` (\url{https://github.com/frbcesab/rwoslite}).
#' 
#' **Important**: an API token must be first obtained and stored locally. See
#' \url{https://github.com/frbcesab/rwoslite} for further information.
#' 
#' The extraction of the title is performed by the function 
#' [title_from_citation] and may fail if the citation reference is malformed.
#' 
#' If the argument `limit` is higher than 1, candidate titles (i.e. titles of
#' the references returned by the WOS Lite API) are compared to the original
#' title, and the DOI of the one that has the best match is returned. This
#' string distance is performed using the function [stringdist::stringdist].
#' 
#' @param x a `character` of length 1. A reference citation written as
#'   "Authors Year Title. Journal...". See [title_from_citation] for further
#'   detail.
#'
#' @param limit an `integer` of length 1. The maximum number of records to test
#'   to identify the match. Must be strictly positive and < 100,0000.
#'
#' @return A `data.frame` with one row and the following 4 columns:
#'   - `search_term`: the cleaned title extracted from the citation reference 
#'     used as the query terms in WOS API;
#'   - `wos_best_title`: the title of the best reference returned by WOS;
#'   - `wos_best_doi`: the DOI corresponding to the best reference returned by 
#'     WOS;
#'   - `wos_string_dist`: the string distance (Optimal string aligment, `osa`)
#'     between the original title and the one returned by WOS. 
#'     See [stringdist::stringdist] for further information.
#' 
#' @export
#' 
#' @examples
#' ## Add an example

search_in_wos <- function(x, limit = 10) {
  
  ## Check arguments ----
  
  is_character(x)
  is_integer(limit)
  
  
  ## Final table ----
  
  data <- data.frame("search_term"     = NA,
                     "wos_best_title"  = NA,
                     "wos_best_doi"    = NA,
                     "wos_string_dist" = NA)
  
  
  ## Extract and clean title ----
  
  title_terms <- title_from_citation(x)
  title_terms <- clean_title(title_terms)
  
  
  if (title_terms != "") {
    
    data$"search_term" <- title_terms
    
    
    ## WOS query ----
    
    search_terms <- paste0("TI=(", title_terms, ")")
    
    wos_results  <- tryCatch(
      rwoslite::wos_get_records(search_terms, limit = limit),
      error = function(e) data.frame())
    
    
    ## Filter results (both DOI and title are required) ----
    
    wos_results <- wos_results[which(!is.na(wos_results$"title") &&
                                     !is.na(wos_results$"doi")), ]
    
    if (nrow(wos_results) > 0) {
        
      wos_dois    <- wos_results$"doi"
      wos_titles  <- wos_results$"title"
      
      
      ## Clean WOS titles the same way ----
      
      wos_strings <- unlist(lapply(wos_titles, clean_title))
      
      
      ## Compute string distances on titles ----
      
      str_dists   <- stringdist::stringdist(tolower(title_terms), 
                                            tolower(wos_strings))
      
      
      ## Identify minimum distance ----
      
      best_match <- which.min(str_dists)
      
      
      ## Store best match ----
      
      data$"wos_best_title"  <- wos_titles[best_match]
      data$"wos_best_doi"    <- wos_dois[best_match]
      data$"wos_string_dist" <- min(str_dists)
    }
  }
  
  data
}
