#' Safe CSV writer
#' 
#' Sorts by key and checks duplicates. Does not overwrite existing file.
#' 
#' @param x the data to write. It gets internally coerced to \code{data.table} preserving col names but not row names.
#' @param file the output file name.
#' @param key one or more column names to check duplicates.
#' @param na the string to use for missing values in the data. 
#' @param compress if \code{TRUE}, the file format is gzipped csv. Default \code{FALSE}.
#' @param ... additional arguments passed to \code{\link[data.table]{fwrite}}.
#' 
#' @return The file name.
#' 
#' @export
#' 
swrite <- function(x, file, key = NULL, na = "", compress = FALSE, ...){
  
  x <- data.table(x, key = key)
  
  if(!is.null(key)) if(any(duplicated(x)))
    stop(sprintf("Duplicated rows for %s", paste(key, collapse = ", ")))
  
  if(file.exists(file)) 
    stop(sprintf("File already exists: %s", file))
  
  if(compress & file.exists(paste0(file, ".gz"))) 
    stop(sprintf("File already exists: %s", paste0(file, ".gz")))
  
  fwrite(x, file = file, na = na, compress = "none", ...)
  
  if(compress) 
    R.utils::gzip(file, overwrite = FALSE)
  
  return(paste0(file, ifelse(compress, ".gz", "")))
  
}
