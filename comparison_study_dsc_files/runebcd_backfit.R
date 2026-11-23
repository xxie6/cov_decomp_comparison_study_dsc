source("ebcd_functions_dsc.R")

ebcd_backfit_given_L <- function(X, init_L, ebnm_fn){
  ebcd_obj <- ebcd_init(X = X)
  ebcd_obj <- ebcd_obj_init_L(ebcd_obj, init_L, ebnm_fn)
  ebcd_fit <- ebcd_backfit(ebcd_obj)
  return(ebcd_fit)
}

ebcd_backfit.wrapper <- function(input, init_L, args){
  # set.seed(args$seed)
  if (is.list(init_L) == FALSE){
    ebcd_fit <- ebcd_backfit_given_L(t(input$Y), init_L, args$ebnm_fn)
    est_L <- ebcd_fit$EL
    est_F <- NULL
  } else {
    m <- length(init_L)
    ebcd_fit <- list()
    est_L <- list()
    est_F <- list()
    for (i in 1:m){
      ebcd_fit[[i]] <- ebcd_backfit_given_L(t(input$split_Y[[i]]),
                                            init_L[[i]],
                                            args$ebnm_fn)
      est_L[[i]] <- ebcd_fit[[i]]$EL
      n <- nrow(input$split_Y[[i]])
      p <- ncol(input$split_Y[[i]])
      if (n < p){
        est_F[[i]] <- transform_ebcd_Z(t(input$split_Y[[i]]), 
                                     ebcd_fit[[i]])
      }
      else {
        est_F[[i]] <- ebcd_fit[[i]]$Z
      }
    }
  }
  return(list(ebcd_fit = ebcd_fit, 
              est_L = est_L, est_F = est_F, 
              method = 'ebcd'))
}
ebcd_data <- ebcd_backfit.wrapper(input, init_L, args)