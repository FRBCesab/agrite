#' Agri-TE project
#' 
#' Extract unique primary studies
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2022/10/13


## Remove missing references ----

full_refs <- refs[which(!is.na(refs$"Reference")), ]


## Extract unique primary studies ----

unique_refs <- full_refs[-which(duplicated(full_refs$"Reference")), ]


## Clean references ----

unique_refs$"Reference" <- gsub("\n", "", unique_refs$"Reference")


## Sort references ----

unique_refs <- unique_refs[order(unique_refs$"Reference"), ]


## Create unique ID ----

unique_refs$"noid" <- 1:nrow(unique_refs)


## Select columns ----

unique_refs <- unique_refs[ , c("noid", "Reference", "DOI")]


## Export unique primary studies ----

writexl::write_xlsx(unique_refs, 
                    path = here::here("data", "derived-data", 
                                      "unique_primary_studies.xlsx"))
