#' Check for non-missing argument of type `character` and length 1
#' 
#' @noRd

is_character <- function(str) {
  
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.character(str)) {
    stop("Argument '", deparse(substitute(str)), "' must be a character", 
         call. = FALSE)
  }
  
  if (length(str) != 1) {
    stop("Argument '", deparse(substitute(str)), "' must be of length 1", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for non-missing argument of type `character` and length > 1
#' 
#' @noRd

is_characters <- function(str) {
  
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.character(str)) {
    stop("Argument '", deparse(substitute(str)), "' must be a character", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for strictly positive `numeric` of length 1
#' 
#' @noRd

is_integer <- function(num) {
  
  if (!is.numeric(num)) {
    stop("Argument '", deparse(substitute(num)), "' must be an integer", 
         call. = FALSE)
  }
  
  if (length(num) != 1) {
    stop("Argument '", deparse(substitute(num)), "' must be of length 1", 
         call. = FALSE)
  }
  
  if (num < 1) {
    stop("Argument '", deparse(substitute(num)), "' must be strictly positive", 
         call. = FALSE)
  }
  
  if (num > 100000) {
    stop("Argument '", deparse(substitute(num)), "' must be < 100,000", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for `curl` output and if a HTTP(S) page exists (status 200)
#' 
#' @noRd

is_status_ok <- function(x) {
  
  if (missing(x)) {
    stop("Argument '", deparse(substitute(x)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.list(x)) {
    stop("Argument '", deparse(substitute(x)), "' must be a list", 
         " (output of `curl::curl_fetch_*()`)",
         call. = FALSE)
  }
  
  if (!("status_code" %in% names(x))) {
    stop("Argument '", deparse(substitute(x)), "' must be a list", 
         " (output of `curl::curl_fetch_*()`)",
         call. = FALSE)
  }
  
  if (x$"status_code" == 503) {
    stop("DOI system is unavailable", call. = FALSE)
  }
  
  if (x$"status_code" != 200) FALSE else TRUE
}



#' Regular expression of DOI
#' 
#' @noRd

doi_regex <- function() {
  "^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$"
}



#' Regular expression of year mentioned in a reference citation
#' 
#' '9999 ' | '9999.' | '(9999)' | '9999z ' | '9999z.' | '(9999z)'
#' 
#' @noRd

year_regex <- function() {
  paste0(c("[0-9]{4}[[:alpha:]]?\\s", "[0-9]{4}[[:alpha:]]?\\.", 
           "\\([0-9]{4}[[:alpha:]]?\\)"), collapse = "|")
}



#' URL of <https://doi.org/>
#' 
#' @noRd

doi_url <- function() {
  "https://doi.org/"
}



#' Check if a DOI has the good structure
#'
#' @noRd

is_valid_doi <- function(doi) {
  
  is_character(doi)
  
  doi <- gsub("\\s", "", doi)
  
  grepl(x = doi, pattern = doi_regex(), perl = TRUE, ignore.case = TRUE)
}



#' Remove numbers in a string
#'
#' @noRd

rm_numbers <- function(str) {
  
  is_character(str)
  gsub("[0-9]", "", str)
}



#' Remove punctuation in a string
#'
#' @noRd

rm_punctuation <- function(str) {
  
  is_character(str)
  gsub("[[:punct:]]", " ", str)
}



#' Replace multiple whitespace by a single whitespace in a string
#'
#' @noRd

rm_multi_spaces <- function(str) {
  
  is_character(str)
  gsub("\\s+", " ", str)
}



#' Remove caesura in a string
#'
#' @noRd

rm_caesura <- function(str) {
  
  is_character(str)
  gsub("-\\s{1,}", "", str)
}



#' Remove leading and trailing whitespace in a string
#'
#' @noRd

rm_lr_spaces <- function(str) {
  
  is_character(str)
  gsub("^\\s|\\s$", "", str) 
}
