#' Clean a title extracted from a citation
#'
#' @description 
#' Cleans a string by removing numbers, caesuras, punctuation, multiple
#' whitespace, leading, and trailing whitespace.
#' 
#' @param x a `character` of length 1. A title extracted from a citation to 
#'   clean.
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
#' ref_title <- title_from_citation(ref)
#' ref_title
#' 
#' clean_title(ref_title)

clean_title <- function(x) {

  is_character(x)
  
  x <- rm_numbers(x)
  x <- rm_caesura(x)
  x <- rm_punctuation(x)
  x <- rm_multi_spaces(x)
  x <- rm_lr_spaces(x)
  
  x
}
