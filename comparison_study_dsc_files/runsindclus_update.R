source("sindclus.R")

sindclus_update_given_L <- function(YYt, init_L, args){
  r <- ncol(init_L)
  n <- nrow(YYt)
  
  # initialize sindclus object
  W <- apply(init_L, 2, max)^2 # assume none of the columns are zero vectors
  P <- t(t(init_L)/sqrt(W))
  
  # pre-processing to make P binary
  P <- round(P)
  
  Q <- P
  
  if (args$additive_term == TRUE){
    if (args$off_diagonal == TRUE){
      c <- sum((YYt - P %*% diag(W) %*% t(Q))*(1 - diag(n)))/sum(1 - diag(n))
    }
    else{
      c <- mean(YYt - P %*% diag(W) %*% t(Q))
    }
  }
  else{
    c <- 0 # C is a matrix where all the values are just one value
  }
  
  obj_func_val <- sum((YYt - (P%*% diag(W) %*% t(Q)))^2)
  obj_func_list <- c(obj_func_val)
  
  sind_obj <- list(P = P, Q = Q, W = W, c = c, 
                   obj_func_val = obj_func_val, obj_func_list = obj_func_list, init_method = 'user_provided_init')
  
  set.seed(args$seed)
  sindclus_fit <- sindclus_update(sind_obj = sind_obj,
                                  S = YYt, 
                                  r = r, 
                                  off_diagonal = args$off_diagonal,
                                  additive_term = args$additive_term,
                                  conv_tol = 1e-6)
  return(sindclus_fit)
}

sindclus_update.wrapper <- function(input, init_L, args){
  if (is.list(init_L) == FALSE){
    sindclus_fit <- sindclus_update_given_L(input$YYt, init_L, args)
    K <- ncol(sindclus_fit$P)
    est_L <- sindclus_fit$P %*% diag(sqrt(sindclus_fit$W), ncol = K) # this is what I edited
  } else {
    m <- length(init_L)
    sindclus_fit <- list()
    est_L <- list()
    for (i in 1:m){
      sindclus_fit[[i]] <- sindclus_update_given_L(input$split_YYt[[i]],
                                                   init_L[[i]],
                                                   args)
      K <- ncol(sindclus_fit[[i]]$P)
      est_L[[i]] <- sindclus_fit[[i]]$P %*% diag(sqrt(sindclus_fit[[i]]$W), ncol = K) # this is what I edited
    }
  }
  return(list(sindclus_fit = sindclus_fit,
              est_L = est_L))
}
sindclus_data <- sindclus_update.wrapper(input, init_L, args)
