library(RMeCab)

Func39 <- function(){
  df <- RMeCabFreq("neko.txt")
  df2 <- df[order(df$Freq, decreasing=T),]
  plot(c(1:length(df2$Freq)),df2$Freq,log="xy",xlab="出現度順位", ylab="出現頻度")
}

Func39()