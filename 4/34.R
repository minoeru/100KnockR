library(RMeCab)
library(magrittr)

Func34 <- function(){
  hoge <- RMeCabText("neko.txt")
  surface <- lapply(1:length(hoge),function(x){hoge[[x]][1]}) %>% unlist()
  pos <- lapply(1:length(hoge),function(x){hoge[[x]][2]}) %>% unlist()
  num <- grep("^の$",surface)
  name <- grep("名詞",pos)
  ans <- intersect(intersect(num - 1,name),intersect(num + 1,name)-2)
  spout <- lapply(1:length(ans),function(x){
    surface[ans[x]:(ans[x]+2)] %>% paste0(.,collapse="") %>% print()
  })
}

Func34()