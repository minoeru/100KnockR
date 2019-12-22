Func12 <- function(txt){
  x <- read.table("hightemp.txt")
  write.table(x[1],"col1.txt",quote=F,col.names=F,row.names=F,sep="")
  write.table(x[2],"col2.txt",quote=F,col.names=F,row.names=F,sep="")
}

Func12("hightemp.txt")
