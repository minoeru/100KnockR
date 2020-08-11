Func7 <- function(x){
  return(paste0(x[1],"時の",x[2],"は",x[3]))
}

Func7(commandArgs(trailingOnly=TRUE))