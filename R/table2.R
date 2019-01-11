#' @title make a faster version of table()
#' @description make a faster version of table() for the case of an input of two integer vectors with no missing values
#' @param x is a vector
#' @param y is a vector
#' @export
#' @examples
#' \dontrun{
#' a <- c(4, 5, 6)
#' table2(a, a)
#' }
table2 <- function(x, y) {
  x_val <- unique(x)
  y_val <- unique(y)
  mat <- matrix(0L, length(x_val), length(y_val))
  for (i in seq_along(x)) {
    mat[which(x_val == x[[i]]), which(y_val == y[[i]])] <-
      mat[which(x_val == x[[i]]),  which(y_val == y[[i]])] + 1L
  }
  dimnames <- list(x_val, y_val)
  names(dimnames) <- as.character(as.list(match.call())[-1])  # R has names for dimnames... :/
  tab <- array(mat, dim = dim(mat), dimnames = dimnames)
  class(tab) <- "table"
  tab
}
