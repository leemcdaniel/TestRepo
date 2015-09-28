#' Weibull Hazard Function
#' 
#' @param t A vector of times
#' @param scale Scale parameter
#' @param shape Shape parameter
#' @return Value of the hazard function
#' @examples
#' hweibull(1:10, 5,3)
#' @export
hweibull <- function(t, scale, shape){
  if(!all(t>0)) stop("Not all t > 0")
  stopifnot(all(t>0), scale>0, shape>0)
  return((shape/scale)*(t/scale)^(shape-1))
}

#' Weibull Mean
#' 
#' @param scale Scale parameter
#' @param shape Shape parameter
#' @return The mean
#' @examples
#' mweibull(1,3)
#' @export
mweibull <- function(scale, shape){
  func <- function(t){
    t*dweibull(t, scale=scale, shape=shape)
  }
  return(integrate(func, 0, Inf)$value)
}


#' Weibull Density
#' 
#' @param t A vector of times
#' @param scale Scale parameter
#' @param shape Shape parameter
#' @return The density
#' @examples
#' dweibull(1:10,1,3)
#' @export
dweibull <- function(t, scale, shape){
  if(!all(t>0)) stop("Not all t > 0")
  stopifnot(all(t>0), scale>0, shape>0)
  return((shape/scale)*((t/scale)^(shape-1)*exp(-(t/scale)^shape)))
}