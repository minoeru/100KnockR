library("magrittr")
hoge <- scan("neko.txt",what = character(),sep = "\n",blank.lines.skip = F)

makedata <- function(data,x,y){
  lapply(1:x,function(z){ data[y + 10 * (z-1)] }) %>% unlist() %>% return()
}

MakeCabocha <- function(n){
  input <- hoge[n]
  system(paste("echo", input, "| cabocha -f1"), intern = T) -> out_text
  gsub("^\\*.*","",out_text) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text
  fuga <- strsplit(out_text,",") %>% unlist()
  
  tmp <- length(fuga) / 10
  surface <- makedata(fuga,tmp,1)
  base <-  makedata(fuga,tmp,8)
  pos <- makedata(fuga,tmp,2)
  pos1 <- makedata(fuga,tmp,3)
  return(data.frame(surface = surface, base = base, pos = pos, pos1 = pos1))
}

ans <- MakeCabocha(3)
print(ans)






