Func11 <- function(txt){
  x <- read.table(txt)
  write.table(x,"hoge.txt",quote=F,col.names=F,row.names=F,sep=" ")
}

Func11("hightemp.txt")
