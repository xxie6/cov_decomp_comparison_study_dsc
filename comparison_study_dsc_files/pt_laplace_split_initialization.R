laplace_split_initialization <- function(S, Kmax, verbose = 2, backfit_maxiter = 500, backfit_tol = NULL){
  # backfitting is important in the tree setting
  
  # fit point-laplace fit with flash
  flash_laplace_fit <- flash_init(data = S, var_type = 0) |>
    flash_set_verbose(verbose = verbose) |>
    flash_greedy(Kmax = Kmax, ebnm_fn = ebnm::ebnm_point_laplace) |>
    flash_backfit(maxiter = backfit_maxiter, tol = backfit_tol) |>
    flash_nullcheck()
  
  # rescale fit so that L and F are of the same scale
  flash_laplace_fit_scaled <- ldf(flash_laplace_fit, 'i')
  LL <- flash_laplace_fit_scaled$L
  ##FF <- flash_laplace_fit_scaled$F
  
  # split into positive and negative components
  LL <- cbind(pmax(LL, 0), pmax(-LL, 0))
  ##FF <- cbind(pmax(FF, 0), pmax(-FF, 0))
  
  # remove columns of zeros
  idx.nonzero <- apply(LL, 2, function(x){return(sum(x^2))}) > 10^(-10)
  LL <- LL[, idx.nonzero]
  
  # refit weights by least squares
  n <- nrow(S)
  llt_vec <- matrix(rep(0, ncol(LL)*n*n), ncol = ncol(LL))
  for (i in 1:ncol(LL)){
    llt_vec[,i] <- c(LL[,i]%*%t(LL[,i]))
  }

  nnlm_fit <- NNLM::nnlm(llt_vec, as.matrix(c(S), ncol = 1), alpha = c(0,0,0))

  indices_keep <- (nnlm_fit$coefficients > 0)
  LL_scaled <- LL[,indices_keep] %*% diag(sqrt(nnlm_fit$coefficients[indices_keep]))
  
  return(LL_scaled)
}
