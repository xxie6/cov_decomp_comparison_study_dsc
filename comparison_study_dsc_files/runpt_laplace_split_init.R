library(flashier)
library(dplyr)
source("pt_laplace_split_initialization.R")
pt_laplace_split_init.wrapper <- function(input, args){
  Kmax <- ceiling(args$K_factor * input$K)
  if (is.list(input[[1]]) == FALSE){
    pt_laplace_split_L <- laplace_split_initialization(S = input$YYt, 
                             Kmax = Kmax)
  } else {
    pt_laplace_split_L <- lapply(input$split_YYt,
                                  laplace_split_initialization,
                                  Kmax = Kmax)
  }
  return(pt_laplace_split_L)
}
pt_laplace_split_L <- pt_laplace_split_init.wrapper(input, args)
