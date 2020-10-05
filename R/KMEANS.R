#' @title KMEANS: K-means clustering function
#'
#' @description This function performs k-means clustering to a data frame in order to classify data observations in the specified number of clusters K.
#'
#' @param x Data matrix
#' @param K Number of clusters
#'
#' @return Vector of cluster assignments, as well as cluster centers.
#' @export
#'
#' @examples
#' KMEANS(x=X.mat,K=5)
#'
#' KMEANS(x=X.mat,K=10)
#'

KMEANS <- function(x, K) {
  centers <- x[sample(1:NROW(x), K),] # Sample some centers, 5 for example
  clusterHistory <- vector(10, mode="list")
  centerHistory <- vector(10, mode="list")
  for(i in 1:10) { #do for 10 iterations
    distsToCenters <- euclid(x, centers)
    clusters <- apply(distsToCenters, 1, which.min)
    centers <- apply(x, 2, tapply, clusters, mean)
    # Saving history
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
  }
  list(clusters=clusterHistory, centers=centerHistory)
}

