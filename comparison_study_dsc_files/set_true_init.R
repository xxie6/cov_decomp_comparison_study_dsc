scale_true_L <- function(input){
  weights <- diag(crossprod(input$FF))
  true_scaled_L <- (1/sqrt(ncol(input$Y)))*input$LL %*% diag(sqrt(weights))
  return(true_scaled_L)
}

true_init.wrapper <- function(input){
  if (is.list(input[[1]]) == TRUE){
    stop("Cannot use true initialization with stability selection")
  }
  true_scaled_L <- scale_true_L(input)
  return(true_scaled_L)
}
true_scaled_L <- true_init.wrapper(input)
