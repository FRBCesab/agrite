## Import data ----

metadata <- get(load(here::here("outputs", 
                                "unique_primary_studies_metadata_bis.RData")))

studies  <- readxl::read_xlsx(here::here("outputs", 
                                         "unique_primary_studies_with_doi_bis.xlsx"),
                              sheet = 1)

studies  <- studies[ , c("noid", "valid_doi")]
studies  <- as.data.frame(studies)


## Detect countries ----

countries_list <- parallel::mclapply(1:length(metadata), function(i) {
  
  if (nrow(metadata[[i]]) == 1) {
    
    if (!is.na(metadata[[i]]$"title")) {
      
      x <- geoparser(metadata[[i]]$"title")
      
      if (nrow(x) > 0) {
        
        x <- data.frame("noid" = i, x)
      }
      
      x
      
    } else {
      
      data.frame()
    }
    
  } else {
    
    data.frame()
  }
  
}, mc.cores = 7)


## Filter match ----

pos <- unlist(lapply(1:length(countries_list), function(i) { 
  if (nrow(countries_list[[i]]) == 0) FALSE else TRUE 
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
                    here::here("outputs", "detected_countries_titles.xlsx"))


writexl::write_xlsx(countries, 
                    here::here("outputs", "detected_countries_titles_summary.xlsx"))

