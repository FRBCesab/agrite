#' Download IPBES World countries boundaries
#'
#' @description
#' This function downloads a spatial vector of world countries boundaries as 
#' defined by the IPBES (available at \url{https://zenodo.org/record/3928281}).
#' A `zip` file is downloaded in the folder `path` (working directory by 
#' default) and files are extracted in the same folder. If this folder doesn't 
#' exist it will be created.
#'
#' **Note:** if you delete the zip` file (after files extraction) and re-run 
#' this function, the zip` file will be downloaded again. It this is your wish 
#' do not forget to use `force = TRUE` to erase previous files.
#'
#' Original Projection System: WGS84
#'
#' @param path a `character` of length 1. The folder inside which the `zip` 
#'   will be downloaded and extracted.
#'   Default is `getwd()`.
#'
#' @param force a `logical`. If `TRUE` previous files will be erased.
#'   Default is `FALSE`.
#'   
#' @return No return value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Download and extract zip file ----
#' get_world_basemap()
#'
#' ## Check ----
#' list.files()
#'
#' ## Re-run function ----
#' get_world_basemap()              # No download (zip file already present)
#' get_world_basemap(force = TRUE)  # Erase previous files
#' }

get_world_basemap <- function(path = ".", force = FALSE) {
  
  ## Check args ----
  
  if (!is.character(path) || length(path) != 1) {
    stop("Argument 'path' must a character of length 1", call. = FALSE)
  }
  
  if (!is.logical(force) || length(force) != 1) {
    stop("Argument 'force' must a logical of length 1", call. = FALSE)
  }
  
  
  ## Check output directory ----
  
  if (force) {
    if (dir.exists(path)) {
      invisible(unlink(path, recursive = TRUE))
    } else {
      dir.create(path, recursive = TRUE)
    }
  } else {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
  
  
  ## Zenodo URL ----
  
  filename <- "ipbes_regions_subregions_shape_1.1.zip"
  
  url <- paste0("https://zenodo.org/record/3928281/files/", filename, 
                "?download=1")
  
  
  ## Download and extract zip file ----
  
  if (!(filename %in% list.files(path))) {
    
    utils::download.file(url, destfile = file.path(path, filename), mode = "wb")
    utils::unzip(zipfile = file.path(path, filename), exdir = path)
  }
  
  invisible(NULL)
}
