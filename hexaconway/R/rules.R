#' Sets of Rules
#'
#' Implementations of eight sets of rules. These are applied to compute the state of the
#' cells/nodes of the next generation.
#'
#' @param neighnumber Matrix where one entry is the sum of the neighbours of the corresponding cell/node.
#' @param attriMatrix Matrix with the actual states of the cells.
#'
#' @return Matrix of the freshly computed cell attributes of the new generation.
#'

#' @describeIn ruleset1 2,3/3
ruleset1 <- function(neighnumber, attriMatrix) {
  ifelse(attriMatrix==0 & neighnumber==3, 1, ifelse(attriMatrix==1 & (neighnumber<2 | neighnumber>3), 0, attriMatrix))
}



#' @describeIn ruleset1 2/1
ruleset2 <- function(neighnumber, attriMatrix) {
  ifelse(attriMatrix==1 & !(neighnumber==2), 0, ifelse(attriMatrix==0 & neighnumber==1, 1, attriMatrix))
}


#' @describeIn ruleset1
#' parity-rule: disregarding the actual state of the cell \cr
#' sum(neighbours) mod 2 == 0 -> 1; \cr
#' sum(neighbours) mod 2 != 0 -> 0)
ruleset3 <- function(neighnumber, attriMatrix) {
  neighnumber <- as.numeric(neighnumber)
  ifelse(neighnumber %% 2 == 0, 1, 0)
}


#' @describeIn ruleset1 inverse parity-rule: disregarding the actual state of the cell \cr
#' sum(neighbours) mod 2 == 0 -> 0; \cr
#' sum(neighbours) mod 2 != 0 -> 1)
ruleset4 <- function(neighnumber, attriMatrix) {
  neighnumber <- as.numeric(neighnumber)
  ifelse(neighnumber %% 2 == 0, 0, 1)
}


#' @describeIn ruleset1 alternative parity-rule: take actual state of the cell into account \cr
#' sum(neighbours+actual state) mod 2 == 0 -> 1;\cr
#' sum(neighbours+actual state) mod 2 != 0 -> 0
ruleset5 <- function(neighnumber, attriMatrix) {
  neighnumber <- as.numeric(neighnumber)
  attriMatrix <- as.numeric(attriMatrix)
  ifelse(neighnumber+attriMatrix %% 2 == 0, 1, 0)
}


#' @describeIn ruleset1 3,4/2
ruleset6 <- function(neighnumber, attriMatrix) {
  ifelse(attriMatrix==0 & neighnumber==2, 1, ifelse(attriMatrix==1 & (!(neighnumber==3) | !(neighnumber==4)), 0, attriMatrix))
}


#' @describeIn ruleset1 3,5/2
ruleset7 <- function(neighnumber, attriMatrix) {
  ifelse(attriMatrix==0 & neighnumber==2, 1, ifelse(attriMatrix==1 & (!(neighnumber==3 | neighnumber==5)), 0, attriMatrix))
}


#' @describeIn ruleset1 3/2,4,5 (as said in one blog no GL-rule - but might be interesting)
ruleset8 <- function(neighnumber, attriMatrix) {
  ifelse(attriMatrix==0 & neighnumber %in% c(2, 4, 5), 1, ifelse(attriMatrix==1 & !(neighnumber==3), 0, attriMatrix))
}
