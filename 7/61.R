library(magrittr)
library(jsonlite)

Func61 <- function(path){
  data <- jsonlite::stream_in(file(path))
  hoge <- as.vector(data[[1]])
  fuga <- as.vector(data[[10]])
  names(fuga) <- hoge
  tmp <- sample(hoge,1)
  fuga[[tmp]] %>% print()
}

Func61("artist.json")