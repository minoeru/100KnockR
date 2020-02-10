library(magrittr)
library(jsonlite)

Func62 <- function(path){
  data <- jsonlite::stream_in(file(path))
  hoge <- as.vector(data[[1]])
  fuga <- as.vector(data[[10]])
  names(fuga) <- hoge
  piyo <- lapply(1:length(fuga),function(x){ifelse(fuga[[x]] == "Japan",x,0)}) %>% unlist() %>% unique()
  piyo <- piyo[-which(piyo %in% NA)]
  piyo <- piyo[-which(piyo %in% 0)]
  length(piyo) %>% print()
}

Func62("artist.json")