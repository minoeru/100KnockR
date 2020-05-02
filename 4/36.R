library(RMeCab)

Func36 <- function(){
  df <- RMeCabFreq("neko.txt")
  df2 <- df[order(df$Freq, decreasing=T),]
  print(paste(df2$Term,df2$Freq))
}

Func36()

