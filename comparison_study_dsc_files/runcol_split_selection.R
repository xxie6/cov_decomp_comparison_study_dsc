source("hungarian_algo_stability_selection.R")
col_split_selection <- function(input, args){
  est_L <- stability_selection_post_processing(input[[1]], 
                                               input[[2]], 
                                               threshold = args$threshold)$final_est
  return(est_L)
}
col_split_results <- col_split_selection(input, args)