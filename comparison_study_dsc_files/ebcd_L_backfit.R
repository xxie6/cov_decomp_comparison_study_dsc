ebcd_fit_L_given_Z <- function(Y, Z, EL, ebnm_fn, maxiter = 1){
  n <- nrow(Y)
  p <- ncol(Y)
  Kmax <- ncol(Z)
  
  # initial estimate for tau
  tau <- prod(dim(Y)) / sum((Y - (Z%*%t(EL)))^2)
  V <- matrix(0, nrow = nrow(EL), ncol = ncol(EL))
  
  for (i in 1:maxiter){
    for (k in 1:Kmax) {
      x <- c(crossprod(Y, Z[, k])) / n
      s <- rep(sqrt(1 / (n * tau)), times=length(x))
      e <- ebnm_fn(x = x, s = s)
      
      EL[, k] <- e$posterior$mean
      V[, k] <- e$posterior$sd^2
    }
    # update tau
    tau <- prod(dim(Y)) / (sum((Y - Z %*% t(EL))^2) + n * sum(V))
  }
  return(EL)
}

ebcd_L_backfit <- function(input, F_est, args){
  n <- nrow(input$Y)
  F_est_orthog <- PolarU(F_est/sqrt(n/2))*sqrt(n)
  L_est_orthog <- t(NNLM::nnlm(x = F_est_orthog , y = t(input$Y))$coefficients)
  ebcd_estimate_L <- ebcd_fit_L_given_Z(t(input$Y), F_est_orthog, L_est_orthog, args$ebnm_fn, maxiter = 1)
  return(ebcd_estimate_L)
}
