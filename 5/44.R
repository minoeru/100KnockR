library("DiagrammeR")
library("magrittr")
hoge <- scan("neko.txt",what = character(),sep = "\n",blank.lines.skip = T)
hoge <- gsub("^　","",hoge) %>% gsub("。|、","",.)

MakeDependency <- function(n){
  input <- hoge[n]
  system(paste("echo", input, "| cabocha"), intern = T) -> morphs
  morphs[1:(length(morphs)-1)] %>% gsub(" |\\||-|D","",.) -> morphs
  system(paste("echo", input, "| cabocha -f1"), intern = T) -> out_text
  gsub("^[^\\*].*","",out_text) %>% .[-which(. %in% "")] %>% gsub("\\* [^ ]* ","",.) %>% gsub("D .*","",.) -> out_text
  dst <- as.numeric(out_text)
  dst <- lapply(1:length(dst),function(x){ifelse(dst[x] != -1,dst[x]+1,-1)}) %>% unlist()
  tmp <- c(1:length(dst))
  srcs <- numeric(length(dst))
  spout <- lapply(1:length(dst),function(x){
    my_tmp <- tmp[dst == x]
    srcs[x] <<- ifelse(length(my_tmp) == 0,"",ifelse(length(my_tmp == 2),paste(my_tmp,collapse = ","),my_tmp))
  }) 
  return(data.frame(morphs = morphs, dst = dst, srcs = srcs))
}

tmp <- MakeDependency(10)
spout <- lapply(1:length(tmp$morphs),function(x){
  ifelse( tmp$dst[x] != -1,paste0(tmp$morphs[x]," -> ",tmp$morphs[tmp$dst[x]],";"),"")
}) %>% unlist() %>% paste0(.,collapse = "")
grViz(paste0("digraph test{",spout,"}")) %>% print()