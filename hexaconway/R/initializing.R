#' Builds network object
#'
#' Initializes object of type network using number of rows r and either probability p or matrix p of starting states of all cells.
#'
#' @param p Either a probability p (number between 0 and 1) or
#' an inputmatrix p with the starting states of the cells (0 or 1).
#' @param r Number of rows r.
#'
#' @return List with network object and indexmatrix a.
#'
#'@export
#'
initializeNet <- function(p, r){
  if (is.matrix(p)){
    m <- p
  } else {
    m <- startconfig(p, r)
  }
  a <- matrix(1:(r*r), byrow=T, nrow=r)
  indmat <- reshape2::melt(a)
  edges <- apply(indmat, 1, neighboursIndices, a=a, r=r)
  # Map combined von allen Vektoren der Liste die Subvektoren nach Index
  e <- do.call(Map, c(c, edges))
  nw <- network::network(x=matrix(unlist(e), ncol=2, byrow=F), directed = F, matrix.type="edgelist", vertex.attrnames = list("health"))
  network::set.vertex.attribute(nw, "health", value=as.vector(t(m)))
  return(list(nw, a))
}









