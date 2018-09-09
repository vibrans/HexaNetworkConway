#' Main function for Playing Conway's Game of Life
#'
#' Starts and runs through the Game of Life.
#'
#' @param p Probability as a number between 0 and 1.
#' @param r Number of rows.
#' @param rule An integer determining the set of rules to be applied in the Game.
#'
#' @export
#'
#' @examples
#' ## example with pattern
#' decahexagon <- matrix(0, nrow=10, ncol=10)
#' decahexagon[c(45, 46, 54, 55, 56, 65, 66)] <- 1
#' decahexagon <- t(decahexagon)
#' pattern <- matrix(0, nrow=20, ncol=20)
#' pattern[5:14, 6:15] <- decahexagon
#' conway(pattern, 20, 2)
#'
#'
#' ## example with probability
#' conway(0.6, 20, 1)
#'
#'
#' ## Gleiter for ruleset 7
#' gleiter1 <- matrix(0, nrow=7, ncol=7, byrow=T)
#' gleiter1[c(10, 12, 22, 24, 26, 28, 30, 34, 45, 47)] <- 1
#' pattern <- matrix(0, nrow=20, ncol=20)
#' pattern[7:13, 1:7] <- gleiter1
#' conway(pattern, 20, 7)
#'
#'
#' ## Gleiter for ruleset 8
#' gleiter2 <- matrix(0, nrow=7, ncol=7, byrow=T)
#' gleiter2[c(10, 12, 15, 21, 24, 26, 37, 39, 41)] <- 1
#' pattern <- matrix(0, nrow=20, ncol=20)
#' pattern[12:18, 8:14] <- gleiter2
#' conway(pattern, 20, 8)
#'
#'
conway <- function(p, r, rule) {
  field <- new.conway(p,r,rule)
  while (max(network::get.vertex.attribute(field$nw, "health")) > 0){
    draw(field$nw, r=field$r)
    if (field$iterations%%5==0) summary.conway(field)
    field$nw <- nextGeneration(field$nw, field$a, field$r, rule)
    field$iterations <- field$iterations + 1
    Sys.sleep(0.05)
  }
}

#' @describeIn conway Function that does not extract information.
#' @export
conway_without_S3 <- function(p, r, rule) {
  o <- initializeNet(p=p, r=r)
  nw <- o[[1]]
  a <- o[[2]]
  while (max(network::get.vertex.attribute(nw, "health")) > 0){
    draw(nw, r=r)
    nw <- nextGeneration(nw, a, r, rule)
    Sys.sleep(0.05)
  }
}
