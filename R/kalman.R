#' Kalman filter
#' 
#' This function returns the filtered series from a vector of measurements.
#' 
#' @details 
#' The underlying model is assumed to be:
#' 
#' \deqn{X_t = X_{t-1} + \epsilon_t} 
#' \deqn{Z_t = X_t + u_t}
#' 
#' where \eqn{X_t} denotes the unobsrved state and \eqn{Z_t} is the corresponding measure. 
#' 
#' @param x numeric vector of measurements
#' @param nmin the minimum number of observations to estimate the rolling variances
#' @param nmax the maximum number of observations to estimate the rolling variances
#' 
#' @returns 
#' numeric vector
#' 
#' @export
#' 
kalman <- function(x, nmin, nmax){
  dx <- x - shift(x)
  dy <- shift(x)
  
  dx[is.na(dy)] <- NA
  dy[is.na(dx)] <- NA
  
  n <- pmax(nmin, pmin(nmax, c(1:length(x))))
  N <- frollsum(!is.na(dx), n, adaptive = TRUE)
  E.dx <- frollmean(dx, n, na.rm = TRUE, adaptive = TRUE)
  E.dy <- frollmean(dy, n, na.rm = TRUE, adaptive = TRUE)
  E.dx.dx <- frollmean(dx*dx, n, na.rm = TRUE, adaptive = TRUE)
  E.dx.dy <- frollmean(dx*dy, n, na.rm = TRUE, adaptive = TRUE)
  
  GGt <- - (N/(N-1)) * (E.dx.dy - E.dx*E.dy)
  HHt <- (N/(N-1)) * (E.dx.dx - E.dx*E.dx) - 2*GGt
  
  GGt[GGt < 0] <- NA 
  HHt[HHt < 0] <- NA
  
  GGt <- nafill(GGt, type = "locf")
  HHt <- nafill(HHt, type = "locf")
  
  nas <- which(is.na(GGt) | is.na(HHt))  
  GGt <- nafill(GGt, type = "nocb")
  HHt <- nafill(HHt, type = "nocb")
  
  n <- length(x)  
  kal <- FKF::fkf(
    a0  = nafill(x, type = "nocb")[1],
    P0  = array(GGt[1], dim = c(1, 1)), 
    dt  = array(0, dim = c(1, 1)), 
    ct  = array(0, dim = c(1, 1)), 
    Tt  = array(1, dim = c(1, 1, 1)), 
    Zt  = array(1, dim = c(1, 1, 1)), 
    HHt = array(HHt, dim = c(1, 1, n)),
    GGt = array(GGt, dim = c(1, 1, n)),
    yt  = array(x, dim = c(1, n))
  )
  
  xtt <- c(kal$att)
  xtt[nas] <- NA
  
  return(xtt)
}
