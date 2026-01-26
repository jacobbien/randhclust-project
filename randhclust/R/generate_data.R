# Generated from _main.Rmd: do not edit by hand

#' Generate samples from a clustering model
#' 
#' Samples X|Y=l ~ N_p(mu_l, sigma^2 * I_p)
#' 
#' @param nums k-vector giving the number of points in each cluster
#' @param mu k-by-p matrix giving the k cluster centers
#' @param sigma within cluster sd
generate_data <- function(nums, mu, sigma = 1) {
  k <- nrow(mu)
  p <- ncol(mu)
  stopifnot(length(nums) == k, nums > 0, sigma > 0)
  x <- matrix(nrow = 0, ncol = p)
  for (l in seq(k)) {
    xl <- matrix(mu[l, ] + sigma * stats::rnorm(nums[l] * p), ncol = p, byrow = TRUE)
    x <- rbind(x, xl)
  }
  list(x = x, clust = rep(1:k, times = nums))
}
