Func4 <- function(tex){
  num <- c(1,5,6,7,8,9,15,16,19)
  tex <- gsub("\\.|,","",tex)
  hoge <- unlist(strsplit(tex," "))
  count <- length(hoge)
  fuga <- c(1:count)
  fuga[num] <- 1
  fuga[-num] <- 2
  piyo <- lapply(1:count,function(x){
    substring(hoge[x],0,fuga[x])
  })
  hogera <- unlist(piyo)
  names(hogera) <- c(1:count)
  return(hogera)
}

Func4("Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can.")
