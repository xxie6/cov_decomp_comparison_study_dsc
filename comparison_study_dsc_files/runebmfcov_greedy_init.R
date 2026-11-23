library(flashier)

ebmfcov_greedy_return_L <- function(S, Kmax, ebnm_fn){
  ebmfcov_greedy_fit <- flash_init(data = S, var_type = 0) |>
    flash_greedy(Kmax = Kmax, ebnm_fn = ebnm_fn)
  ebmfcov_greedy_rescaled_fit <- ldf(ebmfcov_greedy_fit)
  scaled_L <- ebmfcov_greedy_rescaled_fit$L %*% diag(sqrt(ebmfcov_greedy_rescaled_fit$D), ncol = ebmfcov_greedy_fit$n_factors)
  return(scaled_L)
}
ebmfcov_greedy_init.wrapper <- function(input, args){
  Kmax <- ceiling(args$K_factor * input$K)
  if (is.list(input[[1]]) == FALSE){
    ebmfcov_greedy_L <- ebmfcov_greedy_return_L(S = input$YYt, 
                                                     Kmax = Kmax,
                                                     ebnm_fn = args$ebnm_fn)
  } else {
    ebmfcov_greedy_L <- lapply(input$split_YYt, ebmfcov_greedy_return_L,
                                     Kmax = Kmax, ebnm_fn = args$ebnm_fn)
  }
  return(ebmfcov_greedy_L)
}
ebmfcov_greedy_L <- ebmfcov_greedy_init.wrapper(input, args)