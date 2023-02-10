#' Agri-TE project
#' 
#' Check raw data
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2022/10/13


## Open primary studies file ----

refs <- readxl::read_xlsx(here::here("data", "raw-data",
                                     "Ps_Agri_TE_21_12_2022.xlsx"),
                          sheet = 2)

refs <- as.data.frame(refs)


## Check for missing reference ----

length(which(is.na(refs$"Reference")))
length(which(refs$"Reference" == ""))
