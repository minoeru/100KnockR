Func15(5)

Func15 <- function(x){
  hoge <- read.table("hightemp.txt")
  num1 <- nrow(hoge) - x
  num2 <- nrow(hoge)  
  return(hoge[num1:num2,])
}
