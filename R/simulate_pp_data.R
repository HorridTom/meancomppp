#' simulate_pp_data
#'
#' @param n number of paired observations
#' @param n0 number of unpaired observations in the pre-group
#' @param n1 number of unpaired observations in the post-group
#' @param delta effect size
#' @param sigma_0 standard deviation of pre-treatment measurements
#' @param sigma_1 standard deviation of post-treatment measurements
#' @param rho correlation between pre- and post- treatment measurements
#' @param tidy whether to return the simulated data in tidy format
#' @param seed seed passed to set.seed prior to simulation
#'
#' @return
#' @export
#'
#' @examples
simulate_pp_data <- function(n = 4, n0 = 4, n1 = 4, delta = 0.5, sigma_0 = 1, sigma_1 = 2,
                             rho = 0.5, tidy = TRUE, seed = 1234) {
  set.seed(seed)

  mu <- c(0, delta) # Mean
  sigma <- matrix(c(sigma_0^2, sigma_0*sigma_1*rho,
                    sigma_0*sigma_1*rho, sigma_1^2),
                  2) # Covariance matrix

  bvn_sample <- MASS::mvrnorm(n = n + n0 + n1, mu = mu, Sigma = sigma )
  colnames(bvn_sample) <- c("pre","post")

  bvn_sample[c((n+1):(n+n0)),"post"] <- NA_real_
  bvn_sample[c((n+n0+1):(n+n0+n1)),"pre"] <- NA_real_
  bvn_sample <- tibble::as_tibble(bvn_sample)
  bvn_sample <- bvn_sample %>% tibble::rownames_to_column("unit_id")

  if(tidy) {
    bvn_sample %>% tidyr::pivot_longer(-unit_id, names_to = "group", values_to = "value")
  } else {
    bvn_sample
  }
}
