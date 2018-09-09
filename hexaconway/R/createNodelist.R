#' Construction of Edgelists
#'
#' Assigns to every cell the index of its neighbours. And then creates the edgelists that are nessecary for
#' the representation in a network object.
#'
#' @param x One row of molten indexmatrix with index of row, index of column and index of node.
#' @param a Matrix in which entries are indices of nodes.
#'
#' @return A list with two entries: a vector of all indices of 'end 1'-nodes and a vector of all
#' indices of 'end 2'-nodes (end 1 and end 2 because final network is not directed).
#'
#'@export
#'
neighboursIndices <- function(x, a, r){
  x1 <- x[1]
  x2 <- x[2]
  ende2 <- c(a[((x1-1):(x1+1))[(x1-1):(x1+1) <= r],
               (((x2-1):(x2+1))[(x2-1):(x2+1) <= r])])
  # falls ungerade Reihe
  if(x1%%2==1){
    rem <- c(a[c(x1-1, x1+1)[c(x1-1, x1+1) <= r],
               (x2+1)[(x2+1) <= r]], a[x1, x2])
    ende2 <- ende2[!ende2 %in% rem]
  }
  # falls gerade Reihe
  else{
    rem <- c(a[c(x1-1, x1+1)[c(x1-1, x1+1, x1) <= r],
               (x2-1)[(x2-1) <= r]], a[x1, x2])
    ende2 <- ende2[!ende2 %in% rem]
  }
  ende1 <- rep(a[x1, x2], length(ende2))
  edges <- list(ende1, ende2)
  return(edges)
}
