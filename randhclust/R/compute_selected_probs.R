# Generated from _main.Rmd: do not edit by hand

#' Compute selection probabilities on a new data set
#' 
#' These probabilites are used in the integrand of the p-value calculation.
#' 
#' @param x_new observed data matrix
#' @param hc_sequence output of `get_hclust_sequence()` applied to output of
#' `hclust_randomized()`
#' @param dist_func the function that was used to compute the original n by n matrix
#' @param linkage_func the linkage used to measure the dissimilarity
#' between pairs of clusters
#' @param tau
compute_selected_probs <- function(x_new, hc_sequence, dist_func, linkage_func, tau) {
  num_steps <- length(hc_sequence$clusts) # different from nrow(x_new) - 1 if truncated in integrand
  probs_selected <- rep(NA, num_steps)
  
  # this part is much like `hclust_randomized()` in how it updates d_clusts
  # and computes probabilities, but it is different in that it goes with the
  # predetermined sequence of merges
  d <- as.matrix(dist_func(x_new))
  d_clusts <- d
  for (i in seq(num_steps)) {
    # calculate probabilities
    probs <- get_merge_probs(d_clusts, tau)
   
    # get the chosen two clusters from the current clustering
    c1 <- hc_sequence$merges[i, 1]
    c2 <- hc_sequence$merges[i, 2]
    # record probability for chosen pair
    probs_selected[i] <- probs[c1, c2] + probs[c2, c1] # matrix is lower tri
    if (i == num_steps) break

    # update dissimilarities
    d_clusts <- d_clusts[-c(c1, c2), -c(c1, c2), drop = FALSE]
    num_clusts <- length(hc_sequence$clusts[[i]])
    d_new <- c()
    for (j in seq(num_clusts)[-c(c1, c2)]) {
      d_new <- c(d_new, linkage_func(
        hc_sequence$clusts[[i + 1]][[1]], # newly created cluster
        hc_sequence$clusts[[i]][[j]], # remaining clusters
        d
      ))
    }

    # recall that new cluster will be first, i.e. clusts[[i+1]][[1]]:
    d_clusts <- rbind(d_new, d_clusts)
    d_clusts <- cbind(c(0, d_new), d_clusts)
  }
  
  return(probs_selected)
}
