#' Read file as character string
#' 
#' @param file the file to read.
#' 
#' @returns 
#' \code{character} vector
#' 
#' @export
#' 
readFile <- function(file){
  readChar(file, file.info(file)$size)
}
  