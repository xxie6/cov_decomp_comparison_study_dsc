library(flashier)
source("ebmfcov_greedy_refit_alt_init.R")
ebmfcov_greedy_refit_init.wrapper <- function(input, args){
  Kmax <- ceiling(args$K_factor * input$K)
  if (is.list(input[[1]]) == FALSE){
    ebmfcov_greedy_refit_L <- ebmfcov_greedy_refit_alt_init(cov_mat = input$YYt, 
                                                     Kmax = Kmax,
                                                     ebnm_fn = args$ebnm_fn)
  } else {
    ebmfcov_greedy_refit_L <- lapply(input$split_YYt, ebmfcov_greedy_refit_alt_init,
                                     Kmax = Kmax, ebnm_fn = args$ebnm_fn)
  }
  return(ebmfcov_greedy_refit_L)
}
ebmfcov_greedy_refit_L <- ebmfcov_greedy_refit_init.wrapper(input, args)