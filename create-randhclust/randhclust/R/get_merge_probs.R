# Generated from _main.Rmd: do not edit by hand

#' Compute merge probabilities
#'
#' Computes merge probability vector as in Section 2. When tau is 0, puts equal
#' probability on all merges that have minimum dissimilarity.  Probabilities are
#' taken to be 0 on upper triangle and diagonal.
#'
#' @param d a symmetric k-by-k matrix of dissimilarities between clusters
#' @param tau amount of randomization (tau = 0 is without randomization)
get_merge_probs <- function(d, tau) {
  if (tau == 0)
    mat <- d == min(d[lower.tri(d, diag=FALSE)])
  else {
    d_lower <- d[lower.tri(d, diag = FALSE)]
    tau_eff <- tau * mean(d_lower)
   mat <- exp(-d / tau_eff)
  }
  mat[upper.tri(mat, diag=TRUE)] <- 0
  return(mat / sum(mat))
}
