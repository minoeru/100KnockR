Func10 <- function(txt){
  hoge <- read.table(txt)
  return(nrow(hoge))
}

Func10("hightemp.txt")
