source("hungarian_algo_stability_selection.R")
row_split_selection <- function(input, args){
  est_F_results <- stability_selection_post_processing(input[[1]], 
                                               input[[2]], 
                                               threshold = args$threshold)
  return(list(est_F = est_F_results$final_est, pairings = est_F_results$pairings))
}
row_split_results <- row_split_selection(input, args)