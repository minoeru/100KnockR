expand("hightemp.txt")

expand <- function(txt){
  x <- read.table(txt)
  write.table(x,"hoge.txt",quote=F,col.names=F,row.names=F,sep=" ")
}
