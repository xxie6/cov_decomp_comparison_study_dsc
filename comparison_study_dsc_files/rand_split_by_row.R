rand_split_by_row.wrapper <- function(input, args){
  set.seed(args$seed)
  
  n <- nrow(input$Y)
  X_split_by_row <- list()
  XXt_split_by_row <- list()
  partition_idx <- list()
  
  m <- 2
  r <- n %% m
  partition_sizes <- rep(floor(n/m), m)
  if (r != 0){
    partition_sizes[1:r] <- partition_sizes[1:r] + 1
  }
  
  idx.avail <- 1:n
  for (i in 1:m){
    idx.sample <- sample(idx.avail, size = partition_sizes[i])
    X_split_by_row[[i]] <- input$Y[idx.sample, ]
    partition_idx[[i]] <- idx.sample
    XXt_split_by_row[[i]] <- tcrossprod(X_split_by_row[[i]])/ncol(X_split_by_row[[i]])
    idx.avail <- setdiff(idx.avail, idx.sample)
  }
  
  return(list(split_Y = X_split_by_row, split_YYt = XXt_split_by_row, 
              split_idx = partition_idx, split_type = 'row_split',
              K = ncol(input$LL)))
}
data_row_split <- rand_split_by_row.wrapper(input, args)