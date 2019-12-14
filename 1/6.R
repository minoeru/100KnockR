Func6("paraparaparadise","paragraph")

Func6 <- function(text1,text2){
  X <- makeVector(text1)
  Y <- makeVector(text2)
  list("和集合",union(X, Y),"積集合",intersect(X, Y),"差集合",setdiff(X, Y),"Xにseが含まれるか",CheckBoolean(X),"Yにseが含まれるか",CheckBoolean(Y))
}


makeVector <- function(tex){
  num <- nchar(tex)
  hoge <- lapply(2:num,function(x){substring(tex,x-1,x)})
  return(unique(unlist(hoge)))
}

CheckBoolean <- function(fuga){
  ifelse(length(intersect("se",fuga)) == 1,"True","False")
}
