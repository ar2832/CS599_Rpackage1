#' @title Euclidean distance function
#'
#' @description This function calculates the euclidean distance between two points or two data frames/matrices.
#'
#' @param p1 First point or data matrix
#' @param p2 Second point or data matrix
#'
#' @return The euclidean distance between two points
#' @export
#'
#' @examples
#' euclid(p1=X.mat,p2=centers)
#'
#' euclid(p1=centers,p2=centers2)
#'
#'
euclid <- function(p1, p2) {
  distanceMatrix <- matrix(NA, nrow=dim(p1)[1], ncol=dim(p2)[1])
  for(i in 1:nrow(p2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(p1)-p2[i,])^2))
  }
  distanceMatrix
}
