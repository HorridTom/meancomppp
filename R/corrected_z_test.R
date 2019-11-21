#' corrected_z_test
#'
#' @param data partially paired data
#' @param sig_level significance level for the hypothesis test
#'
#' @return list object containing in particular:
#' z_corr, the corrected z statistic for the hypothesis test
#' stat_sig, the result of the hypothesis test
#' @export
#'
#' @examples
#' corrected_z_test(data = simulate_pp_data())
corrected_z_test <- function(data, sig_level = 0.05) {

  # first calculate sample means and variances (Xbar obs 0,1, S^2 obs 0,1)
  sample_stats <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(sample_mean = mean(value, na.rm = TRUE),
                     sample_variance = var(value, use = "complete.obs"))

  wide_data <- data %>%
    tidyr::pivot_wider(names_from = group, values_from = value)

  paired_data <- wide_data %>%
    tidyr::drop_na() %>%
    dplyr::mutate(pre_centred = pre - mean(pre),
                  post_centred = post - mean(post),
                  prod = pre_centred*post_centred)
  # calculate the number of paired and unpaired (in each group) observations
  n <- nrow(paired_data)
  n0 <- wide_data %>% tidyr::drop_na(pre) %>% nrow() - n
  n1 <- wide_data %>% tidyr::drop_na(post) %>% nrow() - n

  S01 <- paired_data %>%
    dplyr::summarise(S01 = (1/(n-1))*sum(prod)) %>%
    dplyr::pull(S01)

  mean0 = sample_stats %>% dplyr::filter(group == "pre") %>% dplyr::pull(sample_mean)
  mean1 = sample_stats %>% dplyr::filter(group == "post") %>% dplyr::pull(sample_mean)
  var0 = sample_stats %>% dplyr::filter(group == "pre") %>% dplyr::pull(sample_variance)
  var1 = sample_stats %>% dplyr::filter(group == "post") %>% dplyr::pull(sample_variance)

  mean_diff = mean0 - mean1

  var_corr = (var0 / (n0 + n)) + (var1 / (n1 + n)) - (2*n*S01 / ((n0 + n)*(n1 + n)))

  z_corr = mean_diff / sqrt(var_corr)

  pval <- 2*pnorm(-abs(z_corr))
  stat_sig <- pval < sig_level

  output <- list()
  output$data <- data
  output$n <- n
  output$n0 <- n0
  output$n1 <- n1
  output$sample_stats <- sample_stats
  output$S01 <- S01
  output$mean_diff <- mean_diff
  output$var_corr <- var_corr
  output$z_corr <- z_corr
  output$stat_sig <- stat_sig
  output$pval <- pval

  output
}
