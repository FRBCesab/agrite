#' Agri-TE project
#' 
#' Export DOIs as a .bib file
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2022/10/13



## Open unique references ----

unique_refs <- readxl::read_xlsx(here::here("outputs", 
                                            "unique_primary_studies_with_doi.xlsx"))


## Get all DOIs ----

dois <- c(unique_refs[which(unique_refs$"is_original_doi_valid"), "DOI"], 
          unique_refs[which(!is.na("best_doi")), "best_doi"])


## Remove NA DOIs ----

dois <- dois[!is.na(dois)]


## Remove DOI URL ----

dois <- gsub("https://doi.org/", "", dois)


## Remove DOIs with bad structure ----

is_dois <- unlist(lapply(dois, is_valid_doi))
dois <- dois[which(is_dois)]


## Get unique DOIs ----

dois <- unique(dois)


## Create .bib ----

doi_to_bib(dois, filename = here::here("outputs", "primary_studies_dois.bib"))
