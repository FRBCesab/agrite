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


## Create PDFs directory ----

path_pdf <- here::here("outputs", "pdfs")

dir.create(path_pdf, showWarnings = FALSE, recursive = TRUE)


for (i in 1:nrow(unique_refs)) {

  ## Clean DOI ----
  
  dois <- c(unique_refs[i, "DOI"], unique_refs[i, "best_doi"])
  dois <- dois[!is.na(dois)]
  dois <- gsub("https://doi.org/", "", dois)
  
  is_dois <- unlist(lapply(dois, is_valid_doi))
  
  dois <- dois[which(is_dois)]
  doi  <- unique(dois)
  
  
  ## Download PDF if possible ----
  
  if (length(doi) == 1) {
    
    get_pdf(doi, path = path_pdf, filename = unique_refs[i, "noid"])
    
    Sys.sleep(sample(seq(0, 5, by = 0.01), 1))
  }
}
