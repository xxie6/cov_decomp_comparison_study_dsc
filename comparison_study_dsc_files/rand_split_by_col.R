rand_split_by_col.wrapper <- function(input, args){
  set.seed(args$seed)
  
  p <- ncol(input$Y)
  X_split_by_col <- list()
  XXt_split_by_col <- list()
  partition_idx <- list()
  
  m <- 2
  r <- p %% m
  partition_sizes <- rep(floor(p/m), m)
  if (r != 0){
    partition_sizes[1:r] <- partition_sizes[1:r] + 1
  }
  
  idx.avail <- 1:p
  for (i in 1:m){
    idx.sample <- sample(idx.avail, size = partition_sizes[i])
    X_split_by_col[[i]] <- input$Y[, idx.sample]
    partition_idx[[i]] <- idx.sample
    XXt_split_by_col[[i]] <- tcrossprod(X_split_by_col[[i]])/ncol(X_split_by_col[[i]])
    idx.avail <- setdiff(idx.avail, idx.sample)
  }
  
  return(list(split_Y = X_split_by_col, split_YYt = XXt_split_by_col, 
              split_idx = partition_idx, split_type = 'column_split',
              K = ncol(input$LL)))
}
data_col_split <- rand_split_by_col.wrapper(input, args)