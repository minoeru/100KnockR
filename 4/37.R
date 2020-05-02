library(RMeCab)

Func37 <- function(){
  df <- RMeCabFreq("neko.txt")
  df2 <- df[order(df$Freq, decreasing=T),]
  tm <- df2$Term[1:10]
  fq <- df2$Freq[1:10]
  par(family = "HiraKakuProN-W3")
  barplot(fq, names.arg=tm)
}

Func37()