#' Find the DOI of a citation using the CrossRef API
#'
#' @description
#' From a citation, extracts the title and searches the corresponding 
#' DOI on the CrossRef database using the CrossRef API 
#' (\url{https://www.crossref.org/documentation/retrieve-metadata/rest-api/}) 
#' and the R package `rcrossref`
#' (\url{https://cran.r-project.org/package=rcrossref}).
#' 
#' **Important**: you need to share your email address with Crossref. See
#' \url{https://docs.ropensci.org/rcrossref/#register-for-the-polite-pool} 
#' for further information.
#' 
#' The extraction of the title is performed by the function 
#' [`title_from_citation`] and may fail if the citation is malformed.
#' 
#' The candidate title (returned by the CrossRef API) is compared to the 
#' original title, and the string distance is performed using the function 
#' [`stringdist::stringdist`] to have an idea of the match.
#' 
#' @param x a `character` of length 1. A citation written as
#'   "Authors Year Title. Journal...". See [`title_from_citation`] for further
#'   detail.
#'
#' @return A `data.frame` with one row and the following 4 columns:
#'   - `search_term`: the cleaned title extracted from the citation used as the
#'     query terms in CrossRef API;
#'   - `cr_best_title`: the title of the best reference returned by CrossRef;
#'   - `cr_best_doi`: the DOI corresponding to the best reference returned by 
#'     CrossRef;
#'   - `cr_string_dist`: the string distance (Optimal string aligment, `osa`)
#'     between the original title and the one returned by CrossRef. 
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
#' search_in_cr(ref)
#' }

search_in_cr <- function(x) {
  
  ## Check arguments ----
  
  is_character(x)
  
  
  ## Final table ----
  
  data <- data.frame("search_term"    = NA,
                     "cr_best_title"  = NA,
                     "cr_best_doi"    = NA,
                     "cr_string_dist" = NA)
  
  
  ## Extract and clean title ----
  
  title_terms <- title_from_citation(x)
  title_terms <- clean_title(title_terms)
  
  
  if (title_terms != "") {
    
    data$"search_term" <- title_terms
    
    
    ## CrossRef query ----
    
    search_terms <- gsub("\\s", "+", title_terms)
    
    cr_results <- rcrossref::cr_works(query = search_terms, sort = "score", 
                                      select = c("DOI", "title"), 
                                      order = "desc", limit = 1)
    cr_results <- cr_results$"data"
    
    if (!("doi" %in% colnames(cr_results)))   cr_results$"doi"   <- NA
    if (!("title" %in% colnames(cr_results))) cr_results$"title" <- NA
    
    
    ## Filter results (both DOI and title are required) ----
    
    cr_results <- cr_results[which(!is.na(cr_results$"title") &&
                                   !is.na(cr_results$"doi")), ]
    
    
    if (nrow(cr_results) > 0) {
      
      
      ## Clean WOS titles the same way ----
      
      cr_strings <- clean_title(cr_results$"title")
      
      
      ## Compute string distances on titles ----
      
      str_dist  <- stringdist::stringdist(tolower(title_terms), 
                                          tolower(cr_strings))
      
      
      ## Store match ----
      
      data$"cr_best_title"  <- cr_results$"title"
      data$"cr_best_doi"    <- cr_results$"doi"
      data$"cr_string_dist" <- str_dist
    }
  }
  
  data
}
