# Generated from _main.Rmd: do not edit by hand

#' Get sequence of clusters and merges from an `hclust` object
#' 
#' @param hc an object of class `hclust`
#' @return `clusts` is a list, where `clusts[[i]]` is the clustering before
#' merge i occurs.  The first cluster in `clusts[[i]]` is the one that was newly
#' created. `merges` is a matrix whose i-th row gives the indices of the
#' two clusters in `clusts[[i]]` being merged  
#' @export
get_hclust_sequence <- function(hc) {
  n <- nrow(hc$merge) + 1
  clusts <- list()
  clusts[[1]] <- as.list(1:n) # initially each leaf is in own cluster
  merges <- matrix(NA, n - 1, 2)
  cl <- list()
  for (i in seq(n - 1)) {
    for (j in 1:2) {
      if (hc$merge[i, j] < 0)
        cl[[j]] <- -hc$merge[i, j]
      else
        cl[[j]] <- clusts[[hc$merge[i, j] + 1]][[1]] # clust created at merge i
    }
    merges[i, ] <- match(cl, clusts[[i]])
    if (i == n - 1) break # no need to form a single cluster clustering
    unaltered_clusters <- clusts[[i]][-merges[i, ]]
    new_cluster <- sort(unlist(cl))
    clusts[[i+1]] <- c(list(new_cluster), unaltered_clusters)
  }
  list(clusts = clusts, merges = merges)
}
