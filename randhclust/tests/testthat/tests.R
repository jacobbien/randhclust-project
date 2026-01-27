# Generated from _main.Rmd: do not edit by hand  
testthat::test_that("compute_observed_statistics() works", {
  # imagine a one-dim example with three points:
  x <- c(-1, 1, 6, 10)
  clust1 <- 1:2
  clust2 <- 3
  # working directly from the formula in the paper:
  scaling = 1/2 +1/1
  bcss <- 2 * 1 / 3 * (0-6)^2
  wcss <- 1 + 1 + 0
  out <- compute_observed_statistics(x, clust1, clust2)
  testthat::expect_equal(out$ratio, (3 - 2)* bcss / wcss)
  testthat::expect_equal(out$a, sum(out$b^2) / sum(out$w^2))
  
  # and a two-dimensional example with four points:
  x <- matrix(c(-1, 1, 6, 6, 10,
                 0, 0, 2, 2, 10),
              ncol = 2)
  clust1 <- 1:2
  clust2 <- 3:4
  scaling = 1/2 +1/2
  # working directly from the formula in the paper:
  bcss <- 2 * 2 / 4 * sum(c(6, 2)^2)
  wcss <- 1 + 1 + 0 + 0
  out <- compute_observed_statistics(x, clust1, clust2)
  testthat::expect_equal(out$ratio, (4 - 2)* bcss / wcss)
  testthat::expect_equal(out$a, sum(out$b^2) / sum(out$w^2))
})

testthat::test_that("get_hclust_sequence() works", {
  x <- c(1, 2,     10, 14,              40)
  n <- length(x)
  hc <- hclust(dist(x))
  out <- get_hclust_sequence(hc)
  
  # check that the clustering at each step is a partition of 1:n:
  for (i in seq(n-1)) testthat::expect_equal(sort(unlist(out$clusts[[i]])), 1:n)

  # check that the newly formed clusters are as expected:
  testthat::expect_equal(out$clusts[[2]][[1]], 1:2)
  testthat::expect_equal(out$clusts[[3]][[1]], c(3, 4))
  testthat::expect_equal(out$clusts[[4]][[1]], 1:4)

  # check that the clusters merged correctly form the new clusters:
  for (i in seq(n - 2)) {
    clust1 <- out$clusts[[i]][[out$merges[i, 1]]]
    clust2 <- out$clusts[[i]][[out$merges[i, 2]]]
    testthat::expect_equal(out$clusts[[i + 1]][[1]], sort(c(clust1, clust2)))
  }
})

testthat::test_that("get_hclust_sequence() works", {
  x <- matrix(c(1, 2,     10, 14,              40), ncol = 1)
  n <- length(x)
  hc <- hclust(dist(x))
  out <- get_hclust_sequence(hc)
  
  # when tau = 0 and original x is used all probs should be 1 when there are 
  # no tied distances:
  testthat::expect_equal(
    compute_selected_probs(x, out, dist, linkage_complete, tau = 0),
    rep(1, n-1))
  
  # when tau = 0 and there are two tied distances:
  x_new <- matrix(c(1, 2, 3,     14,              40), ncol = 1)
  testthat::expect_equal(
    compute_selected_probs(x_new, out, dist, linkage_complete, tau = 0),
    c(0.5, 0, 1, 1))

  # when tau = 0 and there are three tied distances:
  x_new <- matrix(c(1, 2, 3, 4,              40), ncol = 1)
  testthat::expect_equal(
    compute_selected_probs(x_new, out, dist, linkage_complete, tau = 0),
    c(1 / 3, # merging 1-2 is as good as 2-3 or 3-4
      1, # merging 3-4: better than {1,2}-3 and {1,2}-4 for complete linkage
      1, # merging {1,2}-{3,4} is best
      1))
})

