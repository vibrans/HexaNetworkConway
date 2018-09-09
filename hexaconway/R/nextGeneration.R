#' Updating to Next Generation
#'
#' Uses other functions to compute the next generation by respecting the given set of rules and updates
#' the network object.
#'
#' @param nw The network object that represents the state of the actual generation.
#' @param a Indexmatrix.
#' @param r Number of rows.
#' @param rule Number of the ruleset that is to be applied.
#'
#' @return The updated network object.
#'
#'
nextGeneration <- function(nw=nw, a=a, r=r, rule) {
  nn <- matrix(lapply(a, numberNeighbours, nw=nw), byrow=F, nrow=r)
  attri <- matrix(network::get.vertex.attribute(nw, "health"), byrow=T, nrow=r)
  newAttri <- ruler(rule, nn, attri)
  network::set.vertex.attribute(nw, "health", value=as.vector(t(newAttri)))
  return(nw)
}
