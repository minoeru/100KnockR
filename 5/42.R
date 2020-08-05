library("magrittr")
hoge <- scan("neko.txt",what = character(),sep = "\n",blank.lines.skip = T)
hoge <- gsub("^　","",hoge)
hoge <- gsub("。|、","",hoge)

MakeDependency <- function(n){
  input <- hoge[n]
  system(paste("echo", input, "| cabocha"), intern = T) -> morphs
  morphs[1:(length(morphs)-1)] %>% gsub(" |\\||-|D","",.) -> morphs
  system(paste("echo", input, "| cabocha -f1"), intern = T) -> out_text2
  gsub("^[^\\*].*","",out_text2) %>% .[-which(. %in% "")] %>% gsub("\\* [^ ]* ","",.) %>% gsub("D .*","",.) -> out_text2
  
  dst <- as.numeric(out_text2)
  dst <- lapply(1:length(dst),function(x){ifelse(dst[x] != -1,dst[x]+1,-1)}) %>% unlist()
  
  tmp <- c(1:length(dst))
  srcs <- numeric(length(dst))
  spoil <- lapply(1:length(dst),function(x){
    my_tmp <- tmp[dst == x]
    srcs[x] <<- ifelse(length(my_tmp) == 0,"",ifelse(length(my_tmp == 2),paste(my_tmp,collapse = ","),my_tmp))
  }) 
  return(data.frame(morphs = morphs, dst = dst, srcs = srcs))
}

spoil <- lapply(1:10, function(x){
  tmp <- MakeDependency(x)
  spoil <- lapply(1:length(tmp$morphs),function(y){
    if(tmp$dst[y] != -1) paste0(tmp$morphs[y],"\t",tmp$morphs[tmp$dst[y]]) %>% print()
  })
})