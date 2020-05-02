library(RMeCab)

Func38 <- function(){
  df <- RMeCabFreq("neko.txt")
  df2 <- df[order(df$Freq, decreasing=T),]
  fq <- df2$Freq
  fq <- fq[which(fq <= 20)]
  hist(fq,breaks=seq(0,20,1), main="Histogram", xlab="Frequency of appearance",ylab = "Number of word types",  ylim=c(0,6000))
}

Func38()