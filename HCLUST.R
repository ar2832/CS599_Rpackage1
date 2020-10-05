#' @title HCLUST: Hierarchical clustering function
#'
#' @description This function performs hierarchical clustering for a group of data points for a specified number of clusters K.
#'
#' @param data.matrix Data matrix with data observations to cluster
#' @param K Number of clusters
#'
#' @return Vector of clusters assignments.
#' @export
#'
#' @examples
#' HCLUST(data.matrix=X.mat,K=5)
#'
#' HCLUST(data.matrix=X.mat,K=10)
#'
HCLUST <- function(data.matrix, K){
  d <- dist(data.matrix[1:350,], method = 'euclidean')
  if(!is.matrix(d)) d = as.matrix(d)
  N = nrow(d)
  diag(d)=Inf
  n = -(1:N)                       # Tracks group membership
  m = matrix(0,nrow=N-1, ncol=2)   # hclust merge output
  h = rep(0,N-1)                   # hclust height output
  for(j in seq(1,N-1))
  {
    # Find smallest distance and corresponding indices
    h[j] = min(d)
    # We can use == here (I think) because we know we'll get exactly a 0 value.
    i = which(d - h[j] == 0, arr.ind=TRUE)
    # We could get more than one, but we just want to merge one pair, so take 1st.
    i = i[1,,drop=FALSE]
    p = n[i]
    # R's convention is to order each m[j,] pair as follows:
    p = p[order(p)]
    m[j,] = p
    # Agglomerate this pair and all previous groups they belong to
    # into the current jth group:
    grp = c(i, which(n %in% n[i[1,n[i]>0]]))
    n[grp] = j
    # Concoct replacement distances that consolidate our pair using `method`:
    r = apply(d[i,],2,min) #single linkage method
    # Move on to the next minimum distance, excluding current one by modifying
    # the distance matrix:
    d[min(i),] = d[,min(i)] = r
    d[min(i),min(i)]        = Inf
    d[max(i),] = d[,max(i)] = Inf
  }
  # Return something similar to the output from hclust.
  output <-  structure(list(merge = m, height = h,
                            labels = rownames(d),
                            call = match.call(), dist.method = "euclidean"),
                       class = "hclust")
  stats::cutree(output, K)
}
