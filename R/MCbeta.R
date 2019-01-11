#' @title use the Monte Carlo to estimate of the Beta(3, 3)cdf
#' @description Write a function to compute a Monte Carlo estimate of the Beta(3, 3) cdf
#' @description Compare the estimates with the values returned by the pbeta function in R
#' @return return the a Monte Carlo estimate and pbeta function in R for each x.
#' @param x the number of points between 0 and 1
#' @examples
#' \dontrun{
#' x_seq <- seq(.1,.9,by=.1)
#' F_seq <- numeric(length(x_seq))
#' P_seq <- numeric(length(x_seq))
#' for (i in 1:length(x_seq)) {
#'   F_seq[i] <- F(x_seq[i])  # using the F function to estimate the probability
#'   P_seq[i] <- pbeta(x_seq[i],3,3) # using the pbeta function in R to estimate the probability
#' }
#' print(round(rbind(x_seq, F_seq, P_seq), 3))
#' }
#' @export
MCbeta <- function(x)
{
  a <- 3
  b <- 3
  m <- 1e4
  y <- runif(m, min = 0,max = x)
  F.hat <- x*mean(1/beta(a,b)*y^(a-1)*(1-y)^(b-1))
  F.pbeta <- pbeta(x,3,3)
  error <- F.hat-F.pbeta
  return(c(F.hat,F.pbeta,error))
}
