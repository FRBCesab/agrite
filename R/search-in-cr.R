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
