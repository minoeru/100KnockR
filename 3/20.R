library(magrittr)
library(jsonlite)
library(curl)

Func20 <- function(path){
  data <- jsonlite::stream_in(file(path))
  tmp <- lapply(1:length(data[[2]]),function(x){
    ifelse(match(data[[2]][x],"イギリス"),x,0)
  }) %>% unlist()
  tmp <- tmp[-which(tmp %in% NA)]
  data[[1]][tmp] %>% writeLines()
}

Func20("jawiki-country.json")