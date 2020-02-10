library(jsonlite)

Func60 <- function(path){
  data <- jsonlite::stream_in(file(path))
  hoge <- as.vector(data[[1]])
  fuga <- as.vector(data[[10]])
  names(fuga) <- hoge
}

Func60("artist.json")