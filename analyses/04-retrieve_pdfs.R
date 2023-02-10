#' Agri-TE project
#' 
#' Download PDF using the famous website Sc.-H.b.
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2022/12/15



## Open unique references ----

unique_refs <- readxl::read_xlsx(here::here("outputs", 
                                            "unique_primary_studies_with_doi.xlsx"))

unique_refs <- as.data.frame(unique_refs)


## Create PDFs directory ----

path_pdf <- here::here("outputs", "pdfs")

dir.create(path_pdf, showWarnings = FALSE, recursive = TRUE)


for (i in 16532:nrow(unique_refs)) {

  ## Clean DOI ----
  
  dois <- unlist(c(unique_refs[i, "DOI"], unique_refs[i, "best_doi"]))
  dois <- dois[!is.na(dois)]
  dois <- gsub("https://doi.org/", "", dois)
  
  is_dois <- unlist(lapply(dois, is_valid_doi))
  
  
  ## Download PDF if possible ----
  
  if (length(is_dois) > 0) {
    
    dois <- dois[which(is_dois)]
    doi  <- unique(dois)
    doi  <- gsub("\\s", "", doi)

    if (length(doi) == 1) {
    
      get_pdf(doi, path = path_pdf, filename = as.character(unique_refs[i, "noid"]))
    
      Sys.sleep(sample(seq(0, 5, by = 0.01), 1))
    }
  }
}
