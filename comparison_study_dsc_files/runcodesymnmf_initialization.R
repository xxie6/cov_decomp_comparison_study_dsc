library(codesymnmf)

codesymnmf_return_L <- function(S, Kmax, maxiter){
  codesymnmf_L <- codesymnmf(S, r = Kmax, maxiter = maxiter)$H
  return(codesymnmf_L)
}

codesymnmf_init.wrapper <- function(input, args){
  Kmax <- ceiling(args$K_factor * input$K)
  if (is.list(input[[1]]) == FALSE){
    codesymnmf_L <- codesymnmf(input$YYt, r = Kmax, maxiter = 20)$H
  } else {
    codesymnmf_L <- lapply(input$split_YYt, codesymnmf_return_L,
                                     Kmax = Kmax, maxiter = 20)
  }
  return(codesymnmf_L)
}
codesymnmf_L <- codesymnmf_init.wrapper(input, args)