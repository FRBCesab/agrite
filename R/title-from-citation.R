#' Extract the title from a citation
#' 
#' @description 
#' The title must follow the year. This function detects different forms of 
#' years (e.g. `2021 `, `2021.`, `(2021)`) and handles deduplicated years 
#' (e.g. `2021a`, `2021b`, etc.).
#' 
#' The title must end with a dot. If the title contains internal dots, only 
#' the part of the title left to the first dot will be returned.
#' 
#' @param x a `character` of length 1. A citation written as
#'   "Authors Year Title. Journal..."
#' 
#' @return A `character` of length 1 with the extracted title. If the title 
#' cannot be detected, an empty string is returned.
#' 
#' @export
#' 
#' @examples 
#' ref <- paste0("Quainoo, A.K., Wetten, A.C., Allainguillaume, J., 2008. The ",
#'               "effectiveness of somatic embryogenesis in eliminating the ", 
#'               "cocoa swollen shoot virus from infected co- coa trees. ", 
#'               "J. Virol. Methods 149, 91e96.")
#' ref
#' 
#' title_from_citation(ref)

title_from_citation <- function(x) {
  
  is_character(x)
  
  x <- strsplit(x, year_regex())[[1]]
  
  if (length(x) > 1) x <- x[2] else return("")
  
  x <- strsplit(x, "\\.")[[1]]
  
  if (length(x) > 1) return(x[1]) else return("")
}
