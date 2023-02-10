#' Agri-TE project
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2022/10/13



## Install Dependencies (listed in DESCRIPTION) ----

if (!("remotes" %in% installed.packages()[ , "Package"]))
  install.packages("remotes")

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

pkgload::load_all(here::here())


## Create sub-folders ----

dir.create(here::here("data", "derived-data"), showWarnings = FALSE)
dir.create(here::here("outputs"), showWarnings = FALSE)


## Run Project ----

# source(here::here("analyses", "01-check_raw_data.R"))
# source(here::here("analyses", "02-extract_unique_refs.R"))
# source(here::here("analyses", "03-retrieve_dois.R"))
source(here::here("analyses", "04-retrieve_pdfs.R"))
