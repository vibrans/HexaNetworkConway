#' Random Starting Configuraion
#'
#' Creates a random pattern for the starting generation.
#'
#' @param p Probability (number between 0 and 1).
#' @param r Number of rows.
#'
#' @return Matrix with entries that are 0 or 1.
#'
#'@export
#'
startconfig <- function(p, r) {
  v <- rbinom(n=r*r, size=1, prob=p)
  m <- matrix(data=v, nrow=r, ncol=r)
  return(m)
}
