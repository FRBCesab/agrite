#' Convert DOI to BiBTeX
#'
#' @description 
#' From a vector of DOI, creates a BiBTeX expression with only DOI as metadata.
#' No additional metadata (authors, title, etc.) are added. The type of document
#' is set to `article` and a fictitious BiBTeX identifier is created.
#' 
#' The resulting `.bib` file can be imported in a reference management software
#' (Mendeley, Zotero, etc.) to retrieve complete metadata/full text.
#' 
#' @param dois a `character` vector. A vector of DOI.
#' 
#' @param filename a `character` of length 1. The name of the `.bib` file
#'   (with file extension) to create.
#'
#' @return BiBTeX entries with DOI.
#'
#' @export
#'
#' @examples 
#' ## Temporary file ----
#' file_name <- tempfile()
#' 
#' dois <- c("10.1080/03650340.2012.706871", 
#'           "10.1111/j.1744-7348.1970.tb05510.x")
#'           
#' bib_entries <- doi_to_bib(dois, file_name)
#' cat(bib_entries)

doi_to_bib <- function(dois, filename) {
  
  ## Check arguments ----
  
  is_character(filename)
  is_characters(dois)
  
  
  ## Content of the .bib file ----
  
  bib_list <- ""
  
  
  ## Create unique BiBTeX ID ----
  
  noid <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
  
  
  for (i in 1:length(dois)) {
    
    if (is_valid_doi(dois[i])) {
      
      
      ## Append BiBTeX expression (DOI only) ----
      
      bib_list <- c(bib_list, paste0("@article{", "ID", noid + i, ",\n  ", 
                                     "doi={", dois[i], "}", "\n}\n\n"))
    }
  }
  
  
  ## Export .bib ----
  
  bib_list <- paste0(bib_list, collapse = "")
  cat(bib_list, file = filename, append = FALSE)
  
  invisible(bib_list)
}
