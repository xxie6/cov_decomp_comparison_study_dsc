row_split_selection_concat_L <- function(input, pairings, split_idx){
  if (nrow(pairings) == 0){
    est_L_final <- NULL
  } else {
    est_L1_paired <- input[[1]][,pairings[,1], drop = FALSE]
    est_L2_paired_permuted <- input[[2]][,pairings[,2], drop = FALSE]
    est_L_paired <- rbind(est_L1_paired, est_L2_paired_permuted)
    est_L_final <- est_L_paired[order(c(split_idx[[1]], split_idx[[2]])), , drop = FALSE]
  }
  return(est_L_final)
}
row_split_concat_results <- row_split_selection_concat_L(input, pairings, split_idx)
