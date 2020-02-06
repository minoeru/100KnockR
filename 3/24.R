library(magrittr)
library(jsonlite)
library(curl)

Func24 <- function(path){
  data <- jsonlite::stream_in(file(path))
  tmp <- lapply(1:length(data[[2]]),function(x){
    ifelse(match(data[[2]][x],"イギリス"),x,0)
  }) %>% unlist()
  tmp <- tmp[-which(tmp %in% NA)]
  make_data <- data[[1]][tmp]
  made_data <-  strsplit(make_data, "\n") %>% unlist()
  num <- grep("ファイル|File:",made_data)
  lapply(1:length(num),function(x){
    aa <- regexpr(":",made_data[num[x]])
    bb <- regexpr("\\|",substring(made_data[num[x]],aa[1],nchar(made_data[num[x]])))
    substring(made_data[num[x]],(aa[1]+1),(bb[1] + aa[1] -2))
  }) %>% unlist() %>% data.frame() %>% print()
}

Func24("jawiki-country.json")