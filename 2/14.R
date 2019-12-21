Func14(5)

Func14 <- function(x){
  hoge <- read.table("hightemp.txt")
  return(hoge[1:x,])
}
