#' Clean a title extracted from a reference citation
#'
#' @description 
#' Cleans a string by removing numbers, caesuras, punctuation, multiple
#' whitespace, leading, and trailing whitespace.
#' 
#' @param x a `character` of length 1. A title extracted from a reference 
#'   citation to clean.
#'
#' @return A `character` of length 1 with the extracted title. If the title 
#' cannot be detected, an empty string is returned.
#' 
#' @export
#' 
#' @examples 
#' ref <- paste("R Core Team (2022) R: A language and environment for ", 
#'              "statistical computing. R Foundation for Statistical ",
#'              "Computing, Vienna, Austria. URL https://www.R-project.org/.")
#' ref
#' 
#' ref_title <- title_from_citation(ref)
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
