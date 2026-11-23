compute_cosine_sim_matrix <- function(L1, L2){
  norms1 <- apply(L1, 2, function(x){sqrt(sum(x^2))})
  norms1[norms1 < 10^(-15)] <- Inf
  
  norms2 <- apply(L2, 2, function(x){sqrt(sum(x^2))})
  norms2[norms2 < 10^(-15)] <- Inf
  
  L1_normalized <- t(t(L1)/norms1)
  L2_normalized <- t(t(L2)/norms2)
  
  #compute matrix of cosine similarities
  cosine_sim_matrix <- crossprod(L1_normalized, L2_normalized)
  
  return(cosine_sim_matrix)
}

stability_selection_post_processing <- function(L1, L2, threshold=0.99){
  
  # compute similarity
  cosine_sim_matrix <- compute_cosine_sim_matrix(L1, L2)
  
  # find pairings
  # assignment_problem <- lpSolve::lp.assign(cosine_sim_matrix, direction = "max")
  # pairings <- which(assignment_problem$solution == 1, arr.ind = TRUE)
  
  assignment_problem <- RcppHungarian::HungarianSolver(-1*cosine_sim_matrix)
  pairings <- assignment_problem$pairs
  
  # remove things that were not paired
  no_pair <- pairings[,2] == 0
  pairings <- pairings[!(no_pair), , drop = FALSE]
  
  # filter pairings
  pairing_similarities <- cosine_sim_matrix[pairings]
  pairings.keep <- pairing_similarities > threshold
  pairings <- pairings[pairings.keep, , drop = FALSE]
  # K <- nrow(pairings)
  
  # compute final estimate
  if (nrow(pairings) == 0){
    L_final <- NULL
  } else {
    paired_L <- list()
    paired_L[[1]] <- L1[, pairings[,1],drop=FALSE]
    paired_L[[2]] <- L2[, pairings[,2],drop=FALSE]
    paired_L_arr <- simplify2array(paired_L)
    L_final <- apply(paired_L_arr, 1:2, mean)
  }

  return(list(final_est = L_final, pairings = pairings))
}
