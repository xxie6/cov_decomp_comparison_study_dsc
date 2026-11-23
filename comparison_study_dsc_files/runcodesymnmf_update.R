library(codesymnmf)
codesymnmf_update.wrapper <- function(input, init_L, args = NULL){
  if (is.list(init_L) == FALSE){
    codesymnmf_results <- codesymnmf_fit(input$YYt, init_L)
    est_L <- codesymnmf_results$H
  } else {
    m <- length(init_L)
    codesymnmf_results <- list()
    est_L <- list()
    for (i in 1:m){
      codesymnmf_results[[i]] <- codesymnmf_fit(input$split_YYt[[i]],
                                                init_L[[i]])
      est_L[[i]] <- codesymnmf_results[[i]]$H
    }
  }
  return(list(codesymnmf_fit = codesymnmf_results, 
              est_L = est_L))
}
codesymnmf_data <- codesymnmf_update.wrapper(input, init_L)