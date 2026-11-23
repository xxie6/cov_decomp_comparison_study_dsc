library(flashier)
library(dplyr)
gbcd_L_backfit <- function(input, L_est, F_est, args){
  flash_estimate_L <- flash_init(input$Y) %>%
    flash_factors_init(list(L_est, F_est), ebnm_fn = c(args$ebnm_fn, ebnm::ebnm_point_laplace)) %>%
    flash_factors_fix(kset = 1:ncol(F_est), which_dim = 'factors') %>%
    flash_backfit(maxiter = 1) %>%
    flash_nullcheck()
  est_L <- flash_estimate_L$L_pm
  return(est_L)
}