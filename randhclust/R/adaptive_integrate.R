# Generated from _main.Rmd: do not edit by hand

#' Adaptive grid integration for pdf evaluation
#'
#' Performs two-pass adaptive grid refinement to efficiently evaluate a 
#' probability density function. First identifies the main part of support on a 
#' wide grid, then refines evaluation on a narrower grid focused on the 
#' high-density region.
#'
#' @param f A function that takes a single numeric input and returns the 
#'   (possibly unnormalized) probability density at that point.
#' @param observed_target Numeric. The observed value of the test statistic 
#'   for which to compute a p-value.
#' @param wide_grid_min Numeric. The minimum value of the initial wide grid.
#' @param wide_grid_max Numeric. The maximum value of the initial wide grid.
#' @param num_coarse Integer. The number of points to use in the coarse grids 
#'   for both passes.
#' @param num_fine Integer. The number of points to use in the fine grid for 
#'   the first pass. The second pass uses \code{num_fine * 2} points.
#' @param qlow Numeric. The lower quantile for defining the narrower interval 
#'   (default: 0.005).
#' @param qhigh Numeric. The upper quantile for defining the narrower interval 
#'   (default: 0.995).
#' @param buffer Numeric. Factor by which to expand the narrower interval width 
#'   on each side. The final grid spans from `q_low - buffer * width` to 
#'   `q_high + buffer * width`.
adaptive_integrate <- function(f, observed_target, wide_grid_min, wide_grid_max, 
                               num_coarse, num_fine, 
                               qlow = 0.005, qhigh = 0.995, buffer = 3) { 
  
  # Pass 1: Wide grid
  wide_fine_grid <- seq(wide_grid_min, wide_grid_max, length.out = num_fine)
  
  # Evaluate f on fine grid
  f_fine <- sapply(wide_fine_grid, f)
  f_fine <- pmax(0, f_fine)
  
  # Normalize pdf using trapezoidal rule
  dx <- diff(wide_fine_grid)
  total_mass <- sum((f_fine[-1] + f_fine[-length(f_fine)]) / 2 * dx)
  f_fine_norm <- f_fine / total_mass
  
  # Compute CDF using trapezoidal rule
  cdf <- c(0, cumsum((f_fine_norm[-1] + f_fine_norm[-length(f_fine_norm)]) / 2 * dx))
  
  # Remove duplicates for interpolation
  unique_idx <- !duplicated(cdf)
  cdf_unique <- cdf[unique_idx]
  grid_unique <- wide_fine_grid[unique_idx]
  
  # Find narrower interval
  low <- approx(cdf_unique, grid_unique, xout = qlow, rule = 2)$y
  high <- approx(cdf_unique, grid_unique, xout = qhigh, rule = 2)$y
  width <- high - low
  low <- max(wide_grid_min, low - buffer * width)
  high <- min(wide_grid_max, high + buffer * width)
  
  cat("Narrow grid bounds:", low, high, "\n")
  
  # Pass 2: Narrow coarse grid
  narrow_coarse_grid <- seq(low, high, length.out = num_coarse)
  
  # Evaluate f on narrow coarse grid
  f_narrow_coarse <- sapply(narrow_coarse_grid, f)
  
  # Interpolate to narrow fine grid
  narrow_fine_grid <- seq(low, high, length.out = num_fine * 2)
  interp_func_narrow <- splinefun(narrow_coarse_grid, f_narrow_coarse, method = "natural")
  f_narrow_fine <- pmax(0, interp_func_narrow(narrow_fine_grid))
  
  # Compute p-value
  # Normalize the posterior
  posterior <- f_narrow_fine
  posterior_sum <- sum(posterior)
  
  if (posterior_sum == 0 || is.nan(posterior_sum)) {
    # Fallback: use uniform distribution to avoid NaN
    posterior <- rep(1 / length(posterior), length(posterior))
    posterior_sum <- sum(posterior)
  } else {
    posterior <- posterior / posterior_sum
  }
  
  # Calculate p-value: P(statistic >= observed_target)
  p_value <- sum(posterior[narrow_fine_grid >= observed_target])

  # Return results
  return(list(
    p_value = p_value,
    grid = narrow_fine_grid,
    pdf = f_narrow_fine,
    narrow_bounds = c(low, high)
  ))
}
