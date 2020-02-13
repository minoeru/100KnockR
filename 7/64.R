library(magrittr)
library(jsonlite)

Func64 <- function(path){
  data <- jsonlite::stream_in(file(path))
  sum <- length(data[[2]])
  name <- as.vector(data[[1]])
  aliases <- as.vector(data[[11]])
  aliases_name <- lapply(1:sum,function(x){
    ifelse(is.null(aliases[x][[1]]$name),0,aliases[x][[1]]$name)
  }) %>% unlist()
  tags <- as.vector(data[[2]])
  tags_value <- lapply(1:sum,function(x){
    ifelse(is.null(tags[[x]][[2]]),0,tags[[x]][[2]])
  }) %>% unlist()
  rating_value <- as.vector(data[[13]])$value %>% unlist()
  database <<- data.frame(name=name,aliases.name=aliases_name,tags.value=tags_value,rating.value=rating_value)
}

Func64("artist.json")