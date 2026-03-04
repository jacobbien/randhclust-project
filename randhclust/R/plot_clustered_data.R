# Generated from _main.Rmd: do not edit by hand

#' Plot two-dimensional data while showing up to two clusterings
#' 
#' For example, can be used to plot both the true and estimated clusterings.
#' 
#' @param x n-by-2 data matrix
#' @param clust1 n-vector of cluster indices
#' @param clust1_label name of clustering 1 (e.g. "true clustering")
#' @param clust2 n-vector of cluster indices (optional)
#' @param clust2_label name of clustering 2 (e.g. "estimated clustering"); optional
plot_clustered_data <- function(x, clust1, clust1_label = "clust1", clust2 = NULL, clust2_label = "clust2") {
  no_legend <- is.null(clust1_label)
  if (no_legend) clust1_label <- "clust1"
  dat <- tibble::tibble(x1 = x[, 1], x2 = x[, 2], clust1 = clust1)
  dat <- dat %>% 
    dplyr::mutate(!!clust1_label := paste0("clust", clust1))
  if (is.null(clust2)) {
    # show only one clustering
    p <- dat %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$x1,
                                   y = .data$x2,
                                   color = .data[[clust1_label]])) + 
      ggplot2::geom_point() +
      ggplot2::coord_equal()
  }
  else {
    # show two clusterings
    dat <- dat %>% 
      dplyr::mutate(!!clust2_label := paste0("clust", clust2))
    p <- dat %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$x1, y = .data$x2,
                 color = .data[[clust1_label]],
                 shape = .data[[clust2_label]])) + 
      ggplot2::geom_point() +
      ggplot2::coord_equal()
  }
  if (no_legend) p <- p + ggplot2::theme(legend.position = "none")
  return(p)
}
