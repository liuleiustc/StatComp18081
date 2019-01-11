#' @title compute the Gini ratio
#' @description Let X be a non-negative random variable with u=E[X].For a random sample (x_1,x_2,...,x_n) from the distribution of X.
#' @description  the Gini ratio is defined by
#' @param xtype the type of x, xtype=c("rlnorm","runif","rbinom")
#' @export
#' @examples
#' \dontrun{
#' G.est("rlnorm")
#' }
G.est <- function(type = "xtype") {
  n <- 1000
  m <- 1000
  Sum <- numeric(n)
  G.hat <- numeric(m)
  for (j in 1:m) {
    if (type == "rlnorm") {
      general <- rlnorm(n)
    }
    else if (type == "runif") {
      general <- runif(n)
    }
    else if (type == "rbinom") {
      general <- rbinom(n, 100, .1)
    }
    x <- general
    mu.hat <- mean(x)
    x_order <- sort(x)
    for (i in 1:n) {
      Sum[j] <- Sum[j] + (2 * i - n - 1) * x_order[i]
    }
    G.hat[j] <- Sum[j] / (n ^ 2 * mu.hat)
  }
  print(mean(G.hat))
  print(median(G.hat))
  print(quantile(G.hat, seq(.1, .9, length = 9)))
}
