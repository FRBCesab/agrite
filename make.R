#' Agri-TE project
#' 
#' @description 
#' This project contains code for:
#'   - Retrieve DOI from full citation
#'   - Retrieve metadata from DOI
#'   - Retrieve full text (PDF) from DOI
#'   - Extract countries from reference title
#'   - Extract countries in PDF
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2023/03/01



## Install Dependencies (listed in DESCRIPTION) ----

if (!("remotes" %in% installed.packages()[ , "Package"]))
  install.packages("remotes")

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

pkgload::load_all(here::here())


## Create sub-folders ----

dir.create(here::here("data", "derived-data"), showWarnings = FALSE)
dir.create(here::here("outputs"), showWarnings = FALSE)
dir.create(here::here("outputs", "pdf"), showWarnings = FALSE)


## Run Project ----

# source(here::here("analyses", "01-check_raw_data.R"))
# source(here::here("analyses", "02-extract_unique_refs.R"))
# source(here::here("analyses", "03-retrieve_dois.R"))
# source(here::here("analyses", "04-retrieve_pdfs.R"))
# source(here::here("analyses", "03-retrieve_dois_bis.R"))
# source(here::here("analyses", "05-extract_countries_title.R"))
# source(here::here("analyses", "06-extract_countries_pdf.R"))
