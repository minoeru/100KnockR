Func17 <- function(){
  hoge <- read.table("hightemp.txt")
  return(unique(hoge[1]))
}

Func17()
