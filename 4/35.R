library(RMeCab)
library(magrittr)

Func35 <- function(){
  hoge <- RMeCabText("neko.txt")
  surface <- lapply(1:length(hoge),function(x){hoge[[x]][1]}) %>% unlist()
  pos <- lapply(1:length(hoge),function(x){hoge[[x]][2]}) %>% unlist()
  name <- grep("名詞",pos)
  ch_name <- intersect(name,name+1)
  ch_name <- union(ch_name,ch_name-1) %>% sort()
  num <- ch_name[1]
  count <- 0
  spout <- lapply(1:length(ch_name),function(x){
    if(x != length(ch_name) && num+1 == ch_name[x+1]){
      num <<- num + 1
      count <<- count + 1
    }
    else{
      surface[ch_name[x-count]:ch_name[x]] %>% paste0(.,collapse = "") %>% print()
      if(x != length(ch_name)) num <<- ch_name[x+1]
      count <<- 0
    }
  })
}

Func35()


