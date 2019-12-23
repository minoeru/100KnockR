Func18 <- function(){
  hoge <- read.table("hightemp.txt")
  return(hoge[order(hoge$V3,decreasing=T),])
}

Func18()

