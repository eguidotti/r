#' Safe CSV writer
#' 
#' @param x the data to write. It gets internally coerced to \code{data.table} preserving col names but not row names.
#' @param file the output file name.
#' @param key one or more column names to check duplicates.
#' @param na the string to use for missing values in the data. 
#' @param compress if \code{TRUE} then the file format is gzipped csv. Default \code{FALSE}.
#' @param ... additional arguments passed to \code{\link[data.table]{fwrite}}.
#' 
#' @export
#' 
swrite <- function(x, file, key = NULL, na = "", compress = FALSE, ...){
  x <- data.table(x, key = key)
  if(!is.null(key)) if(any(duplicated(x)))
    stop(sprintf("Duplicated rows for %s", paste(key, collapse = ", ")))
  if(file.exists(file)) 
    stop(sprintf("File already exists: %s", file))
  fwrite(x, file = file, na = na, compress = "none", ...)
  if(compress) R.utils::gzip(file, overwrite = FALSE)
}
