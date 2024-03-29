#' Convert regression output to a LaTeX table
#' 
#' @param l a statistical model or a list of statistical models. Lists of models can be specified as \code{l = list(model.1, model.2, ...)}. Different object types can also be mixed.
#' @param file using this argument, the resulting table is written to a file rather than to the \code{R} prompt. The file name can be specified as a character string. 
#' @param scale multiply the regression coefficients (and standard errors) by this value.
#' @param tstat if \code{TRUE}, prints t-stats instead of standard errors.
#' @param ... additional arguments passed to \code{\link[texreg]{texreg}}.
#' 
#' @returns 
#' A character object with a regression table and LaTeX markup. 
#' The object has an additional \code{"texregTable"} class identifier, 
#' which causes the object to be formatted nicely on screen when printed.
#' 
#' @export
#' 
texreg <- function(l, file = NULL, tstat = FALSE, scale = 1, ...){
  
  if(!is.list(l))
    l <- list(l)
  
  for(i in 1:length(l)){
    
    l[[i]] <- texreg::extract(l[[i]])
    l[[i]]@se <- l[[i]]@se * scale
    l[[i]]@coef <- l[[i]]@coef * scale

    if(tstat)
      l[[i]]@se <- l[[i]]@coef / l[[i]]@se 
    
  }
  
  tex <- texreg::texreg(l, ...)
  
  if(tstat)
    tex <- gsub("\\$\\(([^\\s]+?)\\)\\$", "$[\\1]$", tex)
  
  if(!is.null(file))
    print(tex, file = file)
  
  else return(tex)

}
