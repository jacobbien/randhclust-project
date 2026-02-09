# Generated from _main.Rmd: do not edit by hand

#' Integrand in p-value calculation
#' 
#' Creates a perturbed new data matrix based on the observed data and a 
#' perturbation of size r. Then `hc$merge` to reconstruct the possible merges at
#' each step s from 1 to t. Then, computes the probability of this sequence of 
#' merges being chosen for on this perturbed data set.
#' 
#' @param r scalar value at which we evaluate the integrand
#' @param t which step p-value is being calculated at
#' @param x observed data matrix
#' @param hc_sequence output of `get_hclust_sequence()` applied to output of
#' `hclust_randomized()`
#' @param obs_statistics output of `compute_observed_statistics()`
#' @param dist_func the function that was used to compute the original n by n matrix
#' @param linkage_func the linkage used to measure the dissimilarity
#' between pairs of clusters
#' @param tau
integrand <- function(r, t, x, hc_sequence, obs_statistics, dist_func, 
                      linkage_func, tau) {
  p <- ncol(x)
  # truncate sequence to step t:
  hc_sequence$clusts <- hc_sequence$clusts[1:t]
  hc_sequence$merges <- hc_sequence$merges[1:t, , drop = FALSE]
  # get the cluster sizes for the two clusters being tested:
  n1 <- length(hc_sequence$clusts[[t]][[hc_sequence$merges[t, 1]]])
  n2 <- length(hc_sequence$clusts[[t]][[hc_sequence$merges[t, 2]]])
  
  scaling = 1/n1+1/n2
  fr <- sqrt(r / (n1 + n2 - 2 + r))
  x_new <- with(obs_statistics, # all variables other than fr come from list
                sqrt(1+1/a) * fr * b + sqrt((1+a) * (1-fr^2)) * w + (x - b - w))

  probs <- compute_selected_probs(x_new, hc_sequence, dist_func, linkage_func, tau)
  return(df(r, df1 = p, df2 = p * (n1 + n2 - 2)) * prod(probs))
}
