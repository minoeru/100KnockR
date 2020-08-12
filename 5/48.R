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

for(i in 1:10){
  tmp <- MakeDependency(i)
  for(j in 1:length(tmp$morphs)){
    data1 <- tmp$morphs[j]
    system(paste("echo", tmp$morphs[j] , "| cabocha -f1"), intern = T) %>%  gsub("^\\*.*","",.) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text
    if(length(grep("[^代]名詞",out_text)) != 0){
      out_text <- strsplit(out_text[z],",") %>% unlist()
      ans <<- data1
      number <- j
      while(TRUE){
        if(tmp$dst[number] == -1) break
        number <- tmp$dst[number]
        ans <<- paste(ans," -> ",tmp$morphs[number])
      }
      if(length(grep("->",ans)) != 0) ans %>% print()
    }
  }
}