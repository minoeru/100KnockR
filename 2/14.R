args1 = commandArgs(trailingOnly=TRUE)[1]

Func14 <- function(x){
  hoge <- read.table("hightemp.txt")
  return(hoge[1:x,])
}

Func14(as.numeric(args1))
