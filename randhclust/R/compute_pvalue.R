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
  
  
  
#====verify decomposition========
#    n1 <- length(hc_sequence$clusts[[t]][[hc_sequence$merges[t, 1]]])
  #   n2 <- length(hc_sequence$clusts[[t]][[hc_sequence$merges[t, 2]]])
  #   scaling = 1/n1+1/n2
  #   
  #   b = obs_statistics$b
  #   w = obs_statistics$w
  #   bnorm = Matrix::norm(b, "F")
  #   wnorm = Matrix::norm(w, "F")
  #   ratio = obs_statistics$ratio
  #   fr <- sqrt(ratio / (n1 + n2 - 2 + ratio))
  # 
  #   x_check <- sqrt(bnorm^2/scaling + wnorm^2)*(fr * b/bnorm + sqrt(1-fr^2) * w/wnorm) +   (x -b/sqrt(scaling) - w)
  # print(max(abs(x_check - x)) < 1e-8)
# =================================
  
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
   #numerator <- stats::integrate(integrand_func, 0, obs_statistics$ratio)
   #denominator <- stats::integrate(integrand_func, 0, Inf)
   #numerator <- integrate(integrand_func, 0, obs_statistics$ratio, rel.tol=1e-8)
  #denominator <- integrate(integrand_func, 0, Inf, rel.tol=1e-8, subdivisions=2000)
  numerator <- stats::integrate(integrand_func, 0, obs_statistics$ratio)
  extra <- stats::integrate(integrand_func, obs_statistics$ratio, Inf)
  #return(1 - numerator$value / denominator$value)
  
  return(1 - 1 / (1 + extra$value / numerator$value))
}
