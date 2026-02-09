# Generated from _main.Rmd: do not edit by hand

#' Compute selective p-value
#' 
#' Arguments `hc_sequence` and `obs_statistics` if `NULL` are computed from `hc`
#' and `t`.
#' 
#' @param t which step p-value is being calculated at
#' @param x observed data matrix
#' @param hc output of `hclust_randomized()`
#' @param hc_sequence output of `get_hclust_sequence()` applied to output of
#' `hclust_randomized()`
#' @param obs_statistics output of `compute_observed_statistics()`
#' @param dist_func the function that was used to compute the original n by n matrix
#' 
#' @export
compute_pvalue <- function(t, x, hc, hc_sequence = NULL, obs_statistics = NULL, dist_func) {
  stopifnot("hclust_randomized" %in% class(hc))
  
  if (is.null(hc_sequence)) hc_sequence <- get_hclust_sequence(hc)
  if (is.null(obs_statistics)) {
    # at step t, these are the two clusters that get merged:
    clust1 <- hc_sequence$clusts[[t]][[hc_sequence$merges[t, 1]]]
    clust2 <- hc_sequence$clusts[[t]][[hc_sequence$merges[t, 2]]]
    obs_statistics <- compute_observed_statistics(x, clust1, clust2)
  }

  integrand_func <- Vectorize(function(r) {
    integrand(r=r,
              t=t,
              x=x,
              hc_sequence=hc_sequence,
              obs_statistics=obs_statistics,
              dist_func = dist_func,
              linkage_func = hc$linkage_func,
              tau = hc$tau)
  })
  # rather than 1 - I(r)/I(infty), let's try 1 / [1 + I(r) / I(r to inf)]
  numerator <- stats::integrate(integrand_func, 0, obs_statistics$ratio)
  extra <- stats::integrate(integrand_func, obs_statistics$ratio, Inf)
  return(1 - 1 / (1 + extra$value / numerator$value))
}
