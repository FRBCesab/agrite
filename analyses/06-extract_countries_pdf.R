## Import data ----

papers  <- list.files(here::here("outputs", "pdfs"))


studies  <- readxl::read_xlsx(here::here("outputs", 
                                         "unique_primary_studies_with_doi_bis.xlsx"),
                              sheet = 1)

studies  <- studies[ , c("noid", "valid_doi")]
studies  <- as.data.frame(studies)



## Detect countries ----

countries_list <- parallel::mclapply(1:length(papers), function(i) {
  
  texte <- pdftools::pdf_text(here::here("outputs", "pdfs", papers[i]))
  
  x <- geoparser(texte)
      
  if (nrow(x) > 0) x <- data.frame("noid" = gsub("\\.pdf", "", papers[i]), x)
  
  x
  
}, mc.cores = 7)


## Filter match ----

pos <- unlist(lapply(1:length(countries_list), function(i) { 
  if (is.null(countries_list[[i]])) FALSE else TRUE 
}))

countries_df <- do.call(rbind.data.frame, countries_list[which(pos)])


## Add DOI ----


countries_df <- merge(countries_df, studies, by = "noid", all = FALSE)


## Remove duplicates ----

keys <- paste(countries_df$"geographic_entity", countries_df$"valid_doi", sep = "__")

pos <- which(duplicated(keys))
countries_df <- countries_df[-pos, ]


## Summary ----

countries <- rev(sort(table(countries_df$"geographic_entity")))
countries <- as.data.frame(countries)
colnames(countries) <- c("country", "n_studies")


## Export results ----

writexl::write_xlsx(countries_df, 
                    here::here("outputs", "detected_countries_pdfs.xlsx"))


writexl::write_xlsx(countries, 
                    here::here("outputs", "detected_countries_pdfs_summary.xlsx"))

