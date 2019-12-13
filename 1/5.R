Func5("I am an NLPer")

Func5 <- function(tex){
  hoge <- unlist(strsplit(tex," "))
  word_bi_gram <- n_gram(hoge)

  fuga <- gsub(" ","",tex)
  fuga <- unlist(strsplit(piyo,"")) 
  char_bi_gram <- n_gram(fuga)

  return(list(word_bi_gram,char_bi_gram))
}

n_gram <- function(vector){
  num <- length(vector)
  row_num = num -1
  sum = 2 *(num - 1)
  piyo <- matrix(c(1:sum), nrow = row_num, ncol = 2)
  lapply(1:row_num,function(x){
    piyo[x,1] <<- vector[x]
    piyo[x,2] <<- vector[x+1]
  })
  return(piyo)
}
