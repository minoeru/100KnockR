Func13 <- function(){
  hoge1 <- read.table("col1.txt")
  hoge2 <- read.table("col2.txt")
  write.table(data.frame(hoge1,hoge2),"col12.txt",quote=F,col.names=F,row.names=F,sep="\t")
}

Func13()
