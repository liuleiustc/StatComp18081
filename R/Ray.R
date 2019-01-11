#' @title using antithetic variables to generate samples from a Rayleigh distribution.
#' @description Implement a function to generate samples from a Rayleigh distribution,using antithetic variables.
#' @return return the percent reduction in variance of X+X' compared with X_1+X_2 for independent X_1,X_2
#' @param R the number of points by random function
#' @export
#' @examples
#' \dontrun{
#' m <- 1000
#' Ray1 <- Ray2 <- numeric(m)
#' for (i in 1:m) {
#'   Ray1[i] <- Ray(R,FALSE)
#'   Ray2[i] <- Ray(R,TRUE)
#' }
#'
#' print(sd(Ray1))  # X1 and X2 is independent
#' print(sd(Ray2))  # X1 and X2 is antithetic variables
#' print((var(Ray1) - var(Ray2))/var(Ray1))  # the percent reduction in variance
#' }

Ray <- function(R,antithetic = TRUE){
  u <- runif(R/2)
  if (!antithetic) v <- runif(R/2) else
    v <- 1-u
  u <- c(u,v)
  return(mean(sqrt(-2*log(1-u))))
}
