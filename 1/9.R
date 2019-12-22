Change <- function(tex){
  top <- substring(tex,1,1)
  end <- substring(tex,nchar(tex),nchar(tex))
  center <- substring(tex,2,nchar(tex)-1)
  fuga <- nchar(center)
  piyo <- floor(runif(1000,1,fuga+1))
  piyo <- unique(piyo)
  hogera <- lapply(1:fuga,function(x){substring(center,piyo[x],piyo[x])})
  hogera <- unlist(hogera)
  hogera <- paste(hogera,collapse="")
  return(paste0(top,hogera,end,collapse=""))
}

Func9 <- function(tex){
  hoge <- unlist(strsplit(tex," "))
  num <- length(hoge)
  ans <- lapply(1:num,function(x){ifelse(nchar(hoge[x]) < 4,hoge[x],Change(hoge[x]) )})
  ans <- paste(unlist(ans),collapse=" ")
  return(ans)
}

Func9("I couldn’t believe that I could actually understand what I was reading : the phenomenal power of the human mind.")
