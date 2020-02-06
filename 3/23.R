library(magrittr)
library(jsonlite)
library(curl)

Func23 <- function(path){
  data <- jsonlite::stream_in(file(path))
  tmp <- lapply(1:length(data[[2]]),function(x){
    ifelse(match(data[[2]][x],"イギリス"),x,0)
  }) %>% unlist()
  tmp <- tmp[-which(tmp %in% NA)]
  make_data <- data[[1]][tmp]
  made_data <-  strsplit(make_data, "\n") %>% unlist()
  num <- grep("==.*==",made_data)
  made_data2 <- lapply(1:length(num),function(x){
    made_data[num[x]]
  }) %>% unlist()
  text <- lapply(1:length(made_data2),function(x){
    gsub("=","",made_data2[x])
  }) %>% unlist()
  level <- lapply(1:length(made_data2),function(x){
    piyo <- gsub("[^=]","",made_data2[x])
    return(nchar(piyo) / 2 - 1)
  }) %>% unlist()
  data.frame(text,level) %>% print()
}

Func23("jawiki-country.json")