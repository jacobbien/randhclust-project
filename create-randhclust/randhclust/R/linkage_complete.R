# Generated from _main.Rmd: do not edit by hand

#' Complete linkage
#' 
#' @param clust1 a vector giving a subset of `1:n`
#' @param clust2 a vector giving a subset of `1:n`
#' @param d an n-by-n matrix of dissimilarities
linkage_complete <- function(clust1, clust2, d) {
  max(d[clust1, clust2])
}
