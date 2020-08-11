library("magrittr")
Func3 <- function(tex){
  gsub("\\.|,","",tex) %>% strsplit(.," ") %>% unlist() %>% nchar()
}

Func3("Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics.")