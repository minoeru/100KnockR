library(magrittr)
library(tm)
library(SnowballC)
Func52 <- function(place){
  hoge <- scan(place,what = "list",quote = "#")
  num <- grep("\\.|;|:|\\?|!", hoge)
  tmp <- c()
  mold <- toupper(letters)
  spout <- lapply(1:length(num),function(x){
    if(setequal(setdiff(mold,substring(hoge[num[x]+1],1,1)),mold)){
      tmp <<- c(tmp,num[x])
    }
  })
  num <- c(0,num) %>% setdiff(tmp)
  fuga <- lapply(1:(length(num)-1),function(x){
    paste(hoge[(num[x]+1):num[x+1]],collapse = " ")
  }) %>% unlist %>% strsplit(" ")
  hogera <- lapply(1:length(fuga),function(x){
    c(fuga[[x]],"")
  }) %>% unlist
  steam <- Corpus(VectorSource(hogera)) %>%
    tm_map(stemDocument)
  steam <- lapply(1:length(steam),function(x){
    steam[[x]]$content
  }) %>% unlist()
  data.frame(word = hogera, steam = steam) %>% print()
}
Func52("nlp.txt")