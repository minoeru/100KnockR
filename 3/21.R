library(magrittr)
library(jsonlite)
library(curl)

Func21 <- function(path){
  data <- jsonlite::stream_in(file(path))
  tmp <- lapply(1:length(data[[2]]),function(x){
    ifelse(match(data[[2]][x],"イギリス"),x,0)
  }) %>% unlist()
  tmp <- tmp[-which(tmp %in% NA)]
  make_data <- data[[1]][tmp]
  made_data <-  strsplit(make_data, "\n") %>% unlist()
  num <- grep("Category",made_data)
  lapply(1:length(num),function(x){
    made_data[num[x]]
  }) %>% unlist() %>% data.frame() %>%  print()
}

Func21("jawiki-country.json") 