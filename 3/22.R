library(magrittr)
library(jsonlite)
library(curl)

Func22 <- function(path){
  data <- jsonlite::stream_in(file(path))
  tmp <- lapply(1:length(data[[2]]),function(x){
    ifelse(match(data[[2]][x],"イギリス"),x,0)
  }) %>% unlist()
  tmp <- tmp[-which(tmp %in% NA)]
  make_data <- data[[1]][tmp]
  made_data <-  strsplit(make_data, "\n") %>% unlist()
  num <- grep("Category",made_data)
  made_data2 <- lapply(1:length(num),function(x){
    made_data[num[x]]
  }) %>% unlist()
  hoge <- regexpr(":", made_data2)
  fuga <- regexpr("\\||]", made_data2)
  lapply(1:length(hoge),function(x){
    substring(made_data2[x], hoge[x] + 1, fuga[x] - 1)
  }) %>% unlist() %>% data.frame() %>% print()
}

Func22("jawiki-country.json") 