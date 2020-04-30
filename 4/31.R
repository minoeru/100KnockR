library(RMeCab)
library(magrittr)

Func31 <- function(){
  hoge <- RMeCabText("neko.txt")
  surface <- lapply(1:length(hoge),function(x){hoge[[x]][1]}) %>% unlist()
  pos <- lapply(1:length(hoge),function(x){hoge[[x]][2]}) %>% unlist()
  df <- data.frame("表層形" = surface, "品詞" = pos)
  print(with(df, surface[ pos == "動詞"]) %>% unique())
}

Func31()