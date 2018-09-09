#' Number of Living Neighbours
#'
#' Computes the sum of all living neighbours of a cell/node.
#'
#' @param a Index of a cell/node.
#' @param nw Network object.
#'
#' @return Sum of living neighbours (an integer value).
#'
#'@export
#'
numberNeighbours <- function(a, nw=nw){
  sum(network::get.vertex.attribute(nw, "health")[network::get.neighborhood(nw, v=a)])
}
