library(symebcovmf)
run_symebcovmf_fit <- function(YYt, init_L, args){
  symebcovmf_init_obj <- sym_ebcovmf_init(YYt)
  
  init_L_norms <- apply(init_L, 2, function(x){sqrt(sum(x^2))})
  init_L_normalized <- t(t(init_L)/init_L_norms)
  init_lambda <- init_L_norms^2
  
  symebcovmf_init_obj <- sym_ebcovmf_factors_init(YYt, 
                                                  symebcovmf_init_obj, 
                                                  init_L_normalized, 
                                                  init_lambda)
  
  symebcovmf_fit <- refit_lambda(S = YYt, 
                                 symebcovmf_init_obj, 
                                 maxiter = 500)
  symebcovmf_fit <- sym_ebcovmf_backfit(YYt, 
                                        symebcovmf_fit, 
                                        ebnm_fn = args$ebnm_fn, 
                                        backfit_maxiter = 500,
                                        backfit_tol = 10^(-5),
                                        optim_maxiter = 500,
                                        optim_tol = 10^(-5))
  return(symebcovmf_fit)
}
symebcovmf.wrapper <- function(input, init_L, args){
  if (is.list(init_L) == FALSE){
    symebcovmf_fit <- run_symebcovmf_fit(input$YYt, 
                                        init_L,
                                        args)
    K <- ncol(symebcovmf_fit$L_pm)
    est_L <- symebcovmf_fit$L_pm %*% diag(sqrt(symebcovmf_fit$lambda), ncol = K)
  } else {
    m <- length(init_L)
    symebcovmf_fit <- list()
    est_L <- list()
    for (i in 1:m){
      symebcovmf_fit[[i]] <- run_symebcovmf_fit(YYt = input$split_YYt[[i]], 
                                               init_L = init_L[[i]],
                                               args)
      K <- ncol(symebcovmf_fit[[i]]$L_pm)
      est_L[[i]] <- symebcovmf_fit[[i]]$L_pm %*% diag(sqrt(symebcovmf_fit[[i]]$lambda), ncol = K)
    }
  }

  return(list(symebcovmf_fit = symebcovmf_fit, 
              est_L = est_L))
}
symebcovmf_data <- symebcovmf.wrapper(input, init_L, args)
