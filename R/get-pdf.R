#' Download article PDF from Sc.-H.b
#'
#' @description 
#' Downloads article PDF from the famous Sc.-H.b website. The URL of the website
#' must be stored locally as the environment variable `SCIHUB_URL` in the
#' `~/.Renviron`. Use the function [usethis::edit_r_environ()] to edit this
#' file.
#' 
#' @param doi a `character` of length 1. The DOI of the article.
#' 
#' @param download a `logical`. If `TRUE` (default) the PDF is downloaded.
#'   Otherwise the link to the PDF is returned.
#' 
#' @param path a `character` of length 1. The directory to save PDFs. 
#'   Must exist.
#' 
#' @param filename a `character` of length 1. The name of PDF
#'   (without extension).
#' 
#' @param overwrite a `logical`. If `TRUE` and the PDF already exist, the PDF
#'   will be downloaded again. Default if `FALSE`.
#'
#' @return The PDF link (`character` of length 1) or `NULL` (no PDF available).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' doi <- "10.1111/ele.13778"
#' get_pdf(doi, path = ".", filename = "2022-mouillot-ecolet")
#' }

get_pdf <- function(doi, download = TRUE, path = ".", filename = "",
                    overwrite = FALSE) {
  
  
  ## Check args ----
  
  if (missing(doi)) {
    stop("Argument 'doi' is required", call. = FALSE)
  }
  
  check_doi(doi)
  
  if (!is.logical(download) || length(download) != 1) {
    stop("Argument 'download' must be 'TRUE' or 'FALSE'", call. = FALSE)
  }
  
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("Argument 'overwrite' must be 'TRUE' or 'FALSE'", call. = FALSE)
  }
  
  if (download) {
    
    is_character(path)
    is_character(filename)
    
    if (!dir.exists(path)) {
      stop("The directory '", path, "' does not exist.", call. = FALSE)
    }
    
    file_path <- file.path(path, paste0(filename, ".pdf"))
    
    if (file.exists(file_path) && !overwrite) {
      
      message("The PDF file already exists. Use 'overwrite = TRUE' to ",
              "download it again.")
      
      return(invisible(NULL))
    }
  }
  
  
  ## Check env variable ----
  
  base_url <- Sys.getenv("SCIHUB_URL")
  
  if (base_url == "") {
    stop("You must store the Sci-Hub URL in the '~/.Renviron' file under the ",
         "key 'SCIHUB_URL'", call. = FALSE)
  }
  
  
  ## Get HTML page ----
  
  page    <- httr::GET(paste(base_url, doi, sep = "/"))
  content <- httr::content(page, as = "text")
  
  if (length(grep("Unfortunately", content)) == 0) {
    
    ## Extract pdf link ----
    
    pdf_link <- rvest::read_html(page) |> 
      rvest::html_elements("button") |> 
      rvest::html_attr("onclick")
    
    
    ## Clean pdf link ----
    
    pdf_link <- gsub("location.href=", "", pdf_link)
    pdf_link <- gsub("^'|'$", "", pdf_link)
    pdf_link <- gsub("\\?download=true", "", pdf_link)
    
    
    ## Build URL ----
    
    if (length(grep("^//", pdf_link)) == 1) {
      
      pdf_link <- paste0("https:", pdf_link)  
      
    } else {
      
      pdf_link <- paste0(base_url, pdf_link)
    }
    
    
    ## Download PDF ----
    
    if (download) {
        download.file(pdf_link, destfile = file_path)  
    }
    
    return(invisible(pdf_link))
    
  } else {
    
    message("No PDF found.")
    
    return(invisible(NULL))
  }
}
