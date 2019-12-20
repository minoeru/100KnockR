wc("hightemp.txt")

wc <- function(txt){
  hoge <- read.table(txt)
  return(nrow(hoge))
}
