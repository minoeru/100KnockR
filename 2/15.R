args1 = commandArgs(trailingOnly=TRUE)[1]

Func15 <- function(x){
  hoge <- read.table("hightemp.txt")
  num1 <- nrow(hoge) - x
  num2 <- nrow(hoge)  
  return(hoge[num1:num2,])
}

Func15(as.numeric(args1))
