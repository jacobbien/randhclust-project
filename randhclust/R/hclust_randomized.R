# Generated from _main.Rmd: do not edit by hand

#' Randomized Hierarchical Clustering
#' 
#' Performs hierarchical clustering with randomization and outputs an `hclust` 
#' object.
#' @param d a symmetric n-by-n matrix of dissimilarities
#' @param linkage_func a function taking two index sets in {1,...,n}
#' @param tau amount of randomization (tau = 0 is without randomization)
#' @export 
hclust_randomized <- function(d, linkage_func = linkage_complete, tau = 0.1) {
  if ("dist" %in% class(d)) d <- as.matrix(d)
  n <- nrow(d)
  clusts <- as.list(seq(n))
  clust_names <- -seq(n) # naming to match "merge" output of hclust
  d_clusts <- d
  merge <- matrix(0, n - 1, 2)
  height <- numeric(n - 1)
  for (i in seq(n - 1)) {
    # calculate probabilities and choose random pair to merge
    probs <- get_merge_probs(d_clusts, tau)
    probs_vec <- as.vector(probs)
    merge_idx <- sample(seq(length(probs_vec)), size = 1, prob = probs_vec)
    merge_clusts <- arrayInd(merge_idx, .dim = dim(d_clusts))
    c1 <- merge_clusts[1]
    c2 <- merge_clusts[2]
    
    # record height and merge for hclust object
    height[i] <- d_clusts[c1, c2]
    merge[i, ] <- sort(c(clust_names[c1], clust_names[c2]))    
    if (merge[i, 1] < 0 && merge[i, 2] < 0) {
      merge[i, ] <- rev(merge[i, ])
    }
    if (i == n-1) break # done with all n-1 merges
    
    # perform merge
    clusts <- append(clusts[-c(c1, c2)], list(c(clusts[[c1]], clusts[[c2]])))
    clust_names <- c(clust_names[-c(c1, c2)], i)
    d_clusts <- d_clusts[-c(c1, c2), -c(c1, c2), drop = FALSE]
    d_new <- numeric(length(clusts) - 1)
    for (j in seq(length(clusts) - 1)) {
      d_new[j] <- linkage_func(clusts[[length(clusts)]], clusts[[j]], d)
    }

    d_clusts <- rbind(d_clusts, d_new)
    d_clusts <- cbind(d_clusts, c(d_new, 0))
  }
  hc <- structure(list(
    merge = merge,
    height = height,
    labels = as.character(1:n),
    method = paste0("randomized (tau = ", tau, ")"),
    call = match.call(),
    dist.method = "user",
    linkage_func = linkage_func,
    tau = tau
    ), class = c("hclust_randomized", "hclust"))
  hc$order <- stats::order.dendrogram(stats::as.dendrogram(hc))
  return(hc)
}
