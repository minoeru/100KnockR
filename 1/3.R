Func3 <- function(tex){
  tex <- gsub("\\.|,","",tex)
  hoge <- unlist(strsplit(tex," "))
  nchar(hoge)
}

Func3("Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics.")
