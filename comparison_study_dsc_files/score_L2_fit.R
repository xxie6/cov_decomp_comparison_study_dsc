compute_L2_fit <- function(est, dat){
  n <- nrow(dat)
  n_off_diag <- n*(n-1) 
  if(is.null(est) == TRUE){
    score <- sum(dat^2) - sum(diag(dat)^2)
  }else{
  estLLt <- tcrossprod(est)
  score <- sum((dat - estLLt)^2) - sum((diag(dat) - diag(estLLt))^2)
  }
  return(score/n_off_diag)
}
result <- compute_L2_fit(est,dat)
