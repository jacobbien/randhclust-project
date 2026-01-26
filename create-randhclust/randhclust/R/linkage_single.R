# Generated from _main.Rmd: do not edit by hand

#' Single linkage
#' 
#' @param clust1 a vector giving a subset of `1:n`
#' @param clust2 a vector giving a subset of `1:n`
#' @param d an n-by-n matrix of dissimilarities
linkage_single <- function(clust1, clust2, d) {
  min(d[clust1, clust2])
}
