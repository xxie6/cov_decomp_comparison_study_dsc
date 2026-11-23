library(flashier)
library(dplyr)
source("ebmfcov_diag_backfit.R")

flash_seminmf_fixed_L <- function(Y, init_L, args){
 # K <- ncol(init_L)
 # init_F <- tryCatch({t(solve(crossprod(init_L), as.matrix(Matrix::crossprod(init_L, Y))))
 # }, error = function(e){
 #   message("Matrix is singular, initializing F with a random matrix instead.")
 #   matrix(rnorm(K*ncol(Y)), ncol = K)
 # })
 
  init_F <- tryCatch({t(solve(crossprod(init_L), as.matrix(Matrix::crossprod(init_L, Y))))
  }, error = function(e){
    message("LtL is singular, using SVD to solve system.")
    s <- svd(crossprod(init_L))
    D_pinv <- diag(ifelse(s$d > .Machine$double.eps, 1/s$d, 0))
    LtL_pinv <- s$v %*% D_pinv %*% t(s$u)
    t(LtL_pinv %*% as.matrix(Matrix::crossprod(init_L, Y)))
  })

  flash_seminmf_fit <- flash_init(Y) |>
    flash_factors_init(list(init_L, init_F), ebnm_fn = c(args$ebnm_fn, ebnm::ebnm_point_laplace)) |>
    flash_factors_fix(kset = 1:ncol(init_L), which_dim = 'loadings') |>
    flash_backfit()
  return(flash_seminmf_fit)
}

ebmfcov_diag.wrapper <- function(input, init_L, args){
  if (is.list(init_L) == FALSE){
    ebmfcov_diag_fit <- cov_fit_backfit(covmat = input$YYt, 
                             init_L = init_L,
                             ebnm_fn = args$ebnm_fn)
    flash_fit_scaled <- ldf(ebmfcov_diag_fit$fl, 'i')
    est_L <- flash_fit_scaled$L %*% diag(sqrt(flash_fit_scaled$D))
    est_F <- NULL
  } else {
    m <- length(init_L)
    ebmfcov_diag_fit <- list()
    est_L <- list()
    for (i in 1:m){
      ebmfcov_diag_fit[[i]] <- cov_fit_backfit(covmat = input$split_YYt[[i]], 
                                               init_L = init_L[[i]],
                                               ebnm_fn = args$ebnm_fn)
      flash_fit_scaled <- ldf(ebmfcov_diag_fit[[i]]$fl, 'i')
      est_L[[i]] <- flash_fit_scaled$L %*% diag(sqrt(flash_fit_scaled$D))
    }
    if (input$split_type == 'column_split'){
      est_F <- NULL 
    } else {
      est_F <- list()
      for (i in 1:m){
        est_F[[i]] <- flash_seminmf_fixed_L(input$split_Y[[i]],
                                            est_L[[i]],
                                            args)$F_pm
      }
    }
  }
  return(list(ebmfcov_diag_fit = ebmfcov_diag_fit, 
              est_L = est_L,
              est_F = est_F,
              method = 'simplified_gbcd'))
}
ebmfcov_diag_data <- ebmfcov_diag.wrapper(input, init_L, args)
