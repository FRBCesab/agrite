## Rerun DOI detect ----

# load(here::here("outputs", "unique_primary_studies_metadata.RData"))
# 
# unique_refs <- readxl::read_xls(here::here("outputs", "New_PS_Biodiv.xls"))
# unique_refs <- as.data.frame(unique_refs)
# unique_refs <- unique_refs[ , -1]


## Convert to boolean ----

# pos <- which(unique_refs$"is_original_doi_valid" == "FAUX")
# unique_refs$"is_original_doi_valid"[pos] <- FALSE
# 
# pos <- which(unique_refs$"is_original_doi_valid" == "VRAI")
# unique_refs$"is_original_doi_valid"[pos] <- TRUE
# 
# pos <- which(unique_refs$"valid_doi" != "")
# unique_refs[pos, "is_original_doi_valid"] <- TRUE
# 
# unique_refs$"is_original_doi_valid" <- as.logical(unique_refs$"is_original_doi_valid")


## Check and/or get DOI ----

for (i in 1:nrow(unique_refs)) {
  
  cat("Parsing reference", i, "on", nrow(unique_refs), "\r")
  
  if (!is.na(unique_refs[i, "titre"])) {
    
    ## Otherwise, try to find DOI in WOS or CrossRef ----
    
    if (!unique_refs[i, "is_original_doi_valid"]) {
      
      ## Delete wrong provided DOI ----
      
      unique_refs[i, "DOI"] <- NA
      
      
      ## Retrieve DOI from WOS or CR ----
      
      doi_match <- search_doi(paste("9999", unique_refs[i, "titre"], ". xxx"))
      
      
      ## Filter on string distance ----
      
      if (!is.na(doi_match$"search_term")) {
        
        n_char <- nchar(doi_match$"search_term")
        
        if (!is.na(doi_match$"string_dist")) {
          
          if ((doi_match$"string_dist" / n_char) > 0.1) {
            
            doi_match$"search_term" <- NA
            doi_match$"best_title"  <- NA
            doi_match$"best_doi"    <- NA
            doi_match$"string_dist" <- NA
            doi_match$"source"      <- NA
          }
        }
      }
      
      
      ## Store results ----
      
      unique_refs[i, "search_term"] <- doi_match$"search_term"
      unique_refs[i, "best_title"]  <- doi_match$"best_title"
      unique_refs[i, "best_doi"]    <- doi_match$"best_doi"
      unique_refs[i, "string_dist"] <- doi_match$"string_dist"
      unique_refs[i, "source"]      <- doi_match$"source"
      
      
      ## Search for metadata in WOS ----
      
      unique_refs[i, "valid_doi"] <- unique_refs[i, "best_doi"]
      
      if (!is.na(unique_refs[i, "valid_doi"])) {
        
        metadata <- get_metadata(doi = unique_refs[i, "valid_doi"])
        
      } else {
        
        metadata <- data.frame()
      }
      
      metadata_list[[as.character(unique_refs[i, "noid"])]] <- metadata
    }
    
    
    Sys.sleep(sample(seq(0, 5, by = 0.001), 1))
  }
}


## Export unique primary studies with DOI ----

writexl::write_xlsx(unique_refs, 
                    path = here::here("outputs", 
                                      "unique_primary_studies_with_doi_bis.xlsx"))

save(metadata_list, file = here::here("outputs", 
                                      "unique_primary_studies_metadata_bis.RData"))
