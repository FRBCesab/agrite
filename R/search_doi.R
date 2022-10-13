search_doi <- function(x, limit = 10) {
  
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
