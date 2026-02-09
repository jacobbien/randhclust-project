# Generated from _main.Rmd: do not edit by hand

#' Compute selective p-value
compute_pvalue_grid <- function(t, x, hc, hc_sequence = NULL, obs_statistics = NULL, dist_func) {
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
              dist_func = dist,
              linkage_func = hc$linkage_func,
              tau = hc$tau)
  })
  
  results <- adaptive_integrate(
    observed_target = obs_statistics$ratio,
    f = integrand_func,
    wide_grid_min = 0.00001,
    wide_grid_max = 180,
    num_coarse = 200, # making this larger improves approximation, but at cost to time
    num_fine = 2000,
    buffer = 3
    )
  
  return(results$p_value)
}
