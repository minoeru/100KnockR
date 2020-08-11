library("magrittr")

CheckBoolean <- function(fuga){
  ifelse(length(intersect("se",fuga)) == 1,"True","False")
}

Func6 <- function(text1,text2){
  X <- lapply(2:nchar(text1),function(x){substring(text1,x-1,x)}) %>% unlist() %>% unique()
  Y <- lapply(2:nchar(text2),function(x){substring(text2,x-1,x)}) %>% unlist() %>% unique()
  list("和集合",union(X, Y),"積集合",intersect(X, Y),"差集合",setdiff(X, Y),"Xにseが含まれるか",CheckBoolean(X),"Yにseが含まれるか",CheckBoolean(Y))
}

Func6("paraparaparadise","paragraph")