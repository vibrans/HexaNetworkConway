#' Conway Constructor
#'
#' @param p Probability as a number between 0 and 1.
#' @param r Number of rows.
#' @param rule An integer determining the set of rules to be applied in the Game.
#'
#'
new.conway <- function(p, r, rule) {
  o <- initializeNet(p = p, r = r)
  field <- list(
    nw = o[[1]],
    a = o[[2]],
    iterations = 0,
    r = r
  )
  class(field) <- "conway"
  return(field)
}

summary.conway <- function(x) {
  cat("\n")
  cat("Iteration", x$iterations, "\n")
  cat("Anzahl lebender Zellen", sum(network::get.vertex.attribute(x$nw, "health")), "\n")
  cat("Hoehe und Breite des Spielfeldes in Anzahl Zellen: ", x$r, "\n")
}
