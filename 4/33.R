library(RMeCab)
library(magrittr)

Func33 <- function(){
  hoge <- RMeCabText("neko.txt")
  base <- lapply(1:length(hoge),function(x){hoge[[x]][8]}) %>% unlist()
  pos2 <- lapply(1:length(hoge),function(x){hoge[[x]][3]}) %>% unlist()
  df <- data.frame("基本形" = base, "品詞細分類2" = pos2)
  print(with(df, base[pos2 == "サ変接続"]) %>% unique())
}

Func33()
