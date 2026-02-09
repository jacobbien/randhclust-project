# Generated from _main.Rmd: do not edit by hand

#' Compute the test and information for auxiliary statistics
#' 
#' This computes the test statistic, which is the ratio of the squared Frobenius
#' norms of two matrices `b` and `w` times a scaling factor.  This test 
#' statistic is F-distributed when `clust1` and `clust2` are predetermined (i.e.
#' not learned on data being used for test).  But in our setting, the clusters 
#' are learned on the same data.  To properly account for this double dipping, 
#' we will also need the values of some additional statistics, which depend on 
#' `b` and `w`.
#' 
#' @param x n-by-p data matrix
#' @param clust1 a vector giving a subset of `1:n`
#' @param clust2 a vector giving a subset of `1:n`
compute_observed_statistics <- function(x, clust1, clust2) {
  if ("numeric" %in% class(x)) x <- matrix(x, ncol = 1)
  n1 <- length(clust1)
  n2 <- length(clust2)
  if (n1 == 1 & n2 == 1)
    stop("At least one of the tested clusters must be non-singleton")
  scaling <- 1/n1 + 1/n2
  p <- ncol(x)
  xx1 <- scale(x[clust1, , drop = FALSE], center = TRUE, scale = FALSE)
  xx2 <- scale(x[clust2, , drop = FALSE], center = TRUE, scale = FALSE)
  xbar_diff <- attr(xx1, "scaled:center") - attr(xx2, "scaled:center")

  # between <- sum(xbar_diff^2)
  # within <- sum(xx1^2) + sum(xx2^2)
  
  # compute w and b as sparse matrices
  w <- b <- Matrix::Matrix(0, nrow = nrow(x), ncol = p)
  w[clust1, ] <- xx1
  w[clust2, ] <- xx2

  b[clust1, ] <- 1/n1 * matrix(xbar_diff, nrow = n1, ncol = p, byrow = TRUE)
  b[clust2, ] <- -1/n2 * matrix(xbar_diff, nrow = n2, ncol = p, byrow = TRUE)
  b <- b / scaling # scaling = || nu ||^2
  
  a <- sum(b^2) / sum(w^2)
  ratio <- (n1 + n2 - 2) * a
  list(ratio = ratio, b = b, w = w, a = a)
}
