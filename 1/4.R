library(magrittr)

Func4 <- function(tex){
  num <- c(1,5,6,7,8,9,15,16,19)
  tex <- gsub("\\.|,","",tex) %>% strsplit(.," ") %>% unlist()
  tex[num] <- substring(tex[num],0,1)
  tex[-num] <- substring(tex[-num],0,2)
  names(tex) <- c(1:length(tex))
  return(tex)
}

Func4("Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can.")