encode <- Func8("There is nothing either good or bad, but thinking makes it so.")
print(encode)
decode <- Func8(encode)
print(decode)


Func8 <- function(tex){
  num <- length(letters) + 1
  
  hoge <- letters
  names(hoge) <- c(1:26)
  hoge[][[1]]
  
  fuga <- c(1:26)
  names(fuga) <- letters
  fuga[][["c"]]
  
  piyo <- lapply(1:nchar(tex),function(x){
    tmp <- substring(tex,x,x)
    ifelse(tmp == toupper(tmp),tmp,Encode(tmp) ) 
  })
  piyo = paste(unlist(piyo),collapse = "")
  return(piyo)
}

Encode <- function(text){
  tmp_num <- num - fuga[][[text]]
  return(hoge[][[tmp_num]])
}
