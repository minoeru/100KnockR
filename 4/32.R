library(RMeCab)
library(magrittr)

Func32 <- function(){
  hoge <- RMeCabText("neko.txt")
  base <- lapply(1:length(hoge),function(x){hoge[[x]][8]}) %>% unlist()
  pos <- lapply(1:length(hoge),function(x){hoge[[x]][2]}) %>% unlist()
  df <- data.frame("基本形" = base, "品詞" = pos)
  print(with(df, base[ pos == "動詞"]) %>% unique())
}

Func32()