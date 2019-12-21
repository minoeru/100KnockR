args1 = commandArgs(trailingOnly=TRUE)[1]
num <- as.numeric(args1)
hoge <- read.table("hightemp.txt")
max <- nrow(hoge)
count <- ceiling(max / num)

tmp <- lapply(1:count,function(x){
  begin <- 1 + num * ( x - 1 )
  end <- num * x
  ifelse( x != count, print(hoge[begin:end,]) , print(hoge[begin:max,]) )
})

