library(magrittr)
Func50 <- function(place){
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
  lapply(1:(length(num)-1),function(x){
    paste(hoge[(num[x]+1):num[x+1]],collapse = " ")
  }) %>% unlist %>% print()
}

Func50("nlp.txt")