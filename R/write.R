#' Safe CSV writer
#' 
#' @param x the data to write. It gets internally coerced to \code{data.table} preserving col names but not row names.
#' @param file the output file name.
#' @param key one or more column names to check duplicates.
#' @param na the string to use for missing values in the data. 
#' @param compress if \code{TRUE}, the file format is gzipped csv. Default \code{FALSE}.
#' @param archive if \code{TRUE}, saves the \code{file} in a directory named with the current date in the format YYYYMMDD. Default \code{FALSE}.
#' @param verbose if \coe{TRUE}, prints \code{\link[base]{file.info}} on success. Default \code{FALSE}.
#' @param ... additional arguments passed to \code{\link[data.table]{fwrite}}.
#' 
#' @export
#' 
swrite <- function(x, file, key = NULL, na = "", compress = FALSE, archive = FALSE, verbose = FALSE, ...){
  
  x <- data.table(x, key = key)
  
  if(!is.null(key)) if(any(duplicated(x)))
    stop(sprintf("Duplicated rows for %s", paste(key, collapse = ", ")))
  
  if(archive){
    dir <- file.path(dirname(file), format(Sys.Date(), "%Y%m%d"))
    dir.create(dir, showWarnings = FALSE)
    file <- file.path(dir, basename(file))
  }
  
  if(file.exists(file)) 
    stop(sprintf("File already exists: %s", file))
  
  if(compress & file.exists(paste0(file, ".gz"))) 
    stop(sprintf("File already exists: %s", paste0(file, ".gz")))
  
  fwrite(x, file = file, na = na, compress = "none", ...)
  
  if(compress) 
    R.utils::gzip(file, overwrite = FALSE)
  
  if(verbose) 
    print(file.info(paste0(file, ifelse(compress, ".gz", ""))))
  
}
