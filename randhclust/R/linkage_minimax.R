# Generated from _main.Rmd: do not edit by hand

#' Minimax linkage
#' 
#' @param clust1 a vector giving a subset of `1:n`
#' @param clust2 a vector giving a subset of `1:n`
#' @param d an n-by-n matrix of dissimilarities
linkage_minimax <- function(clust1, clust2, d) {
  both <- c(clust1, clust2)
  min(apply(d[both, both], 1, max))
}
