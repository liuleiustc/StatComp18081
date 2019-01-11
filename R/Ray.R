#' @title generate samples from a Rayleigh distribution.
#' @description Implement a function to generate samples from a Rayleigh distribution,using antithetic variables.
#' @param x is the number of samples
#' @param r is the number of random data by runif ()
#' @return the vector of generating from the Rayleigh distribution.
#' @export
#' @examples
#' \dontrun{
#' x <- seq(0.1, 2.5, length = 10)
#' samples <- Ray(x)
#' }

Ray <- function(x, r = 10000){
  antithetic = TRUE
  y <- runif(r/2)
  if(!antithetic) v <- runif(r/2)
  else
    v <- 1-y
  y <- c(y, v)
  cdf <- numeric(length(x))
  for(i in 1:length(x)){
    g <- (y*x[i]^2)*exp(-(y*x[i]^2)/(2*(2^2)))
    cdf[i] <-  1/(2^2)*mean(g)
  }
  cdf
}
