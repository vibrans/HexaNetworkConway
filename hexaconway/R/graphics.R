#' Graphical Display
#'
#' Displays a generation graphically on a hexagonal grid.
#'
#' @param nw Network object.
#' @param r Number of rows.
#'
#' @return Grid of the actual generation.
#'
#'@export
#'
draw <- function(nw, r){
  plotrix::color2D.matplot(matrix(network::get.vertex.attribute(nw, "health"), nrow=r, byrow=T), extremes=c("black", "green"), do.hex=TRUE,
                  border="white", axes = F, xlab = "", ylab = "" )
}
