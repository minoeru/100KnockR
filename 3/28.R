library(magrittr)
library(jsonlite)
library(curl)

Func28 <- function(path){
  data <- jsonlite::stream_in(file(path))
  tmp <- lapply(1:length(data[[2]]),function(x){
    ifelse(match(data[[2]][x],"イギリス"),x,0)
  }) %>% unlist()
  tmp <- tmp[-which(tmp %in% NA)]
  make_data <- data[[1]][tmp]
  made_data <-  strsplit(make_data, "\n") %>% unlist()
  place = c( grep("基礎情報",made_data) + 1 , grep("reference",made_data) - 1 )
  dddd <- lapply(place[1]:place[2],function(x){
    ifelse(grep("=",made_data[x]) == 1,substring(made_data[x],2,nchar(made_data[x])),0)
  }) %>% unlist() %>% gsub("'{2,5}","",.) %>%  gsub("\\[\\[[^\\[]*\\||\\[\\[|\\]\\]","",.) %>% gsub("<ref.*>|<br />|<br/>","",.) %>%  gsub("\\{\\{.*\\|.*\\||\\}\\}","",.)
  text <- lapply(1:length(dddd), function(x){
    substring(dddd[x],1,(regexpr("=",dddd[x]) -1))
  }) %>% unlist()
  ans <- lapply(1:length(dddd), function(x){
    substring(dddd[x],(regexpr("=",dddd[x]) + 1),nchar(dddd[x]))
  }) %>% unlist()
  names(ans) <- text
  print(ans)
}

Func28("jawiki-country.json")
