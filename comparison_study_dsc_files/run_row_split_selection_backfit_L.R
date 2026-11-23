source('nnls_L_estimation.R')
source('gbcd_L_backfit.R')
source("ebcd_functions_dsc.R")
source('ebcd_L_backfit.R')
source('flash_normalf_L_backfit.R')

row_split_selection_backfit_L <- function(F_est, input, method, args){
  Y <- rbind(input$split_Y[[1]], input$split_Y[[2]])[order(c(input$split_idx[[1]], input$split_idx[[2]])), ]
  if (is.null(F_est) == TRUE){
    est_L_final <- NULL
  } else {
    nnls_L_est <- nnls_L_estimation(list(Y=Y), F_est)
    if (method == 'simplified_gbcd') {
      est_L_final <- gbcd_L_backfit(list(Y=Y), nnls_L_est, F_est, args)
    } else if (method == 'ebcd'){ 
      est_L_final <- ebcd_L_backfit(list(Y=Y), F_est, args)
    } else if (method == 'flash_normalf'){
      est_L_final <- flash_normalf_L_backfit(list(Y=Y), nnls_L_est, F_est, args)
    }
  }
  return(est_L_final)
}
row_split_backfit_results <- row_split_selection_backfit_L(F_est, input, method, args)
