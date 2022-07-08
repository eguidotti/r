#' Factor: Expensive minus Cheap
#' 
#' @param spread the effective bid-ask spread
#' @param sdret the standard deviation of returns
#' @param ret the return at time T+1
#' @param by the time index
#' @param weight the weight for value-weighted portfolios
#' @param nyse logical vector representing whether the stock is listed in NYSE. It is used for the computation of breakpoints.
#' 
#' @return Factor returns for each \code{by}-time
#' 
#' @export
#' 
factorEMC <- function(spread, sdret, ret, by, weight = 1, nyse = TRUE){
  
  x <- factorIML(illiq = spread, sdret = sdret, ret = ret, by = by, weight = weight, nyse = nyse)
  
  setnames(x, old = "IML", new = "EMC")
  return(x)
  
}


#' Factor: Illiquid minus Liquid
#' 
#' @param illiq the illiquidity measure
#' @param sdret the standard deviation of returns
#' @param ret the return at time T+1
#' @param by the time index
#' @param weight the weight for value-weighted portfolios
#' @param nyse logical vector representing whether the stock is listed in NYSE. It is used for the computation of breakpoints.
#' 
#' @return Factor returns for each \code{by}-time
#' 
#' @export
#' 
factorIML <- function(illiq, sdret, ret, by, weight = 1, nyse = TRUE){
  
  x <- na.omit(data.table(
    ILLIQ = illiq, 
    SDRET = sdret,
    RET = ret, 
    W = weight, 
    NYSE = nyse,
    BY = by, 
    key = "BY"))
  
  x[, PTF.SDRET := ntile(SDRET, n = c(0, 0.3, 0.7, 1), y = SDRET[NYSE]), by = BY]
  x[, PTF.ILLIQ := ntile(ILLIQ, n = 5, y = ILLIQ[NYSE]), by = .(BY, PTF.SDRET)]
  
  x <- x[!is.na(PTF.SDRET) & !is.na(PTF.ILLIQ)]
  x <- x[, .(RET = weighted.mean(RET, w = W)), by = .(BY, PTF.SDRET, PTF.ILLIQ)]
  x <- x[, .(IML = mean(RET[PTF.ILLIQ==5] - RET[PTF.ILLIQ==1])), by = BY]
  
  setkey(x, "BY")
  return(x)  
  
}


#' Factor: Small minus Big
#' 
#' @param me the market equity as of last June
#' @param beme the book to market ratio
#' @param ret the return at time T+1
#' @param by the time index
#' @param weight the weight for value-weighted portfolios
#' @param nyse logical vector representing whether the stock is listed in NYSE. It is used for the computation of breakpoints.
#' 
#' @return Factor returns for each \code{by}-time
#' 
#' @export
#' 
factorSMB <- function(me, beme, ret, by, weight = 1, nyse = TRUE){
  
  x <- na.omit(data.table(
    ME = me, 
    BEME = beme,
    RET = ret, 
    W = weight,
    NYSE = nyse,
    BY = by, 
    key = "BY"))
  
  x[, PTF.SIZE := ntile(ME, n = 2, y = ME[NYSE]), by = BY]
  x[, PTF.BEME := ntile(BEME, n = c(0, 0.3, 0.7, 1), y = BEME[NYSE]), by = BY]
  
  x <- x[!is.na(PTF.SIZE) & !is.na(PTF.BEME)]
  x <- x[, .(RET = weighted.mean(RET, w = W)), by = .(BY, PTF.SIZE, PTF.BEME)]
  x <- x[, .(SMB = mean(RET[PTF.SIZE==1] - RET[PTF.SIZE==2])), by = BY]
  
  setkey(x, "BY")
  return(x)
  
}


#' Factor: High minus Low
#' 
#' @param me the market equity as of last June
#' @param beme the book to market ratio
#' @param ret the return at time T+1
#' @param by the time index
#' @param weight the weight for value-weighted portfolios
#' @param nyse logical vector representing whether the stock is listed in NYSE. It is used for the computation of breakpoints.
#' 
#' @return Factor returns for each \code{by}-time
#' 
#' @export
#' 
factorHML <- function(me, beme, ret, by, weight = 1, nyse = TRUE){
  
  x <- na.omit(data.table(
    ME = me, 
    BEME = beme,
    RET = ret, 
    W = weight,
    NYSE = nyse,
    BY = by, 
    key = "BY"))
  
  x[, PTF.SIZE := ntile(ME, n = 2, y = ME[NYSE]), by = BY]
  x[, PTF.BEME := ntile(BEME, n = c(0, 0.3, 0.7, 1), y = BEME[NYSE]), by = BY]
  
  x <- x[!is.na(PTF.SIZE) & !is.na(PTF.BEME)]
  x <- x[, .(RET = weighted.mean(RET, w = W)), by = .(BY, PTF.SIZE, PTF.BEME)]
  x <- x[, .(HML = mean(RET[PTF.BEME==3] - RET[PTF.BEME==1])), by = BY]
  
  setkey(x, "BY")
  return(x)
  
}


#' Factor: Market
#' 
#' @param ret the return at time T
#' @param rf the risk-free rate at time T
#' @param by the time index
#' @param weight the weight for the value-weighted market
#' 
#' @return Factor returns for each \code{by}-time
#' 
#' @export
#' 
factorMKT <- function(ret, rf, by, weight = 1){
  
  x <- na.omit(data.table(
    RET = ret,
    RF = rf,
    W = weight,
    BY = by, 
    key = "BY"))
  
  x <- x[, .(MKT = weighted.mean(RET - RF, w = W)), by = BY]
  
  setkey(x, "BY")
  return(x)
  
}
