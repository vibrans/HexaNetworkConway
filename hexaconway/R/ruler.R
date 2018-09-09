#' Ruler
#'
#' Calls a function for computing the states of the nodes/cells in the next generation corresponding to the indicated ruleset.
#'
#' @param setruleset Number of the set of rules that should be applied to compute the next generation.
#' The default is Conway's original ruleset 2,3 / 3 (here no. 1).
#' @param nn Number of living neighbours of a specific node/cell.
#' @param attriMatrix Matrix where entries are attributes of the corresponding node/cell.
#'
#' @return Matrix with the freshly computed attributes of the cells/nodes.
#'
#'
ruler <- function(setruleset=1, nn, attriMatrix) {
  if (setruleset==1){ruleset1(nn, attriMatrix)}
  else if (setruleset==2){ruleset2(nn, attriMatrix)}
  else if (setruleset==3){ruleset3(nn, attriMatrix)}
  else if (setruleset==4){ruleset4(nn, attriMatrix)}
  else if (setruleset==5){ruleset5(nn, attriMatrix)}
  else if (setruleset==6){ruleset6(nn, attriMatrix)}
  else if (setruleset==7){ruleset7(nn, attriMatrix)}
  else if (setruleset==8){ruleset8(nn, attriMatrix)}
  else {print("The ruler does not know this rule!!! He breaks down.")}
}
