library(RMeCab)
library(magrittr)

Func30 <- function(){
  hoge <- RMeCabText("neko.txt")
  surface <- lapply(1:length(hoge),function(x){hoge[[x]][1]}) %>% unlist()
  base <- lapply(1:length(hoge),function(x){hoge[[x]][8]}) %>% unlist()
  pos <- lapply(1:length(hoge),function(x){hoge[[x]][2]}) %>% unlist()
  pos1 <- lapply(1:length(hoge),function(x){hoge[[x]][6]}) %>% unlist()
  df <- data.frame("表層形" = surface, "基本形" = base, "品詞" = pos, "品詞細分類1" = pos1)
  print(df)
}

Func30()

