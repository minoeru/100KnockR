library(magrittr)

n_gram <- function(vector){
  hoge <- paste(vector,vector,sep=" ",collapse=" ") %>% strsplit(.," ") %>% unlist()
  hoge[c(-1,-length(hoge))] %>% matrix(.,ncol=2,byrow = T) %>% return()
}

Func5 <- function(tex){
  word_bi_gram <- strsplit(tex," ") %>% unlist() %>% n_gram()
  char_bi_gram <- gsub(" ","",tex) %>% strsplit(.,"") %>% unlist() %>% n_gram()
  return(list(word_bi_gram,char_bi_gram))
}

Func5("I am an NLPer")