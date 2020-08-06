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

spoil <- lapply(1:10,function(x){
  tmp <- MakeDependency(x)
  
  spoil <- lapply(1:length(tmp$morphs),function(y){
    # 原型の抽出
    data1 <- tmp$morphs[y]
    system(paste("echo", data1 , "| cabocha -f1"), intern = T) -> tmp1
    gsub("^\\*.*","",tmp1) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text
    spout <- lapply(1:length(out_text),function(z){
      if(length(grep("[^助容]動詞",out_text[z])) != 0){
        out_text <- strsplit(out_text[z],",") %>% unlist()
        return(out_text[8])
      }
    }) %>% unlist()
    
    # 動詞がなければ終了
    if(is.null(spout)) return()
    #係り元がなければ終了
    if(tmp$srcs[y] %>% as.character() == "") return()
    
    # 係り元
    tmp$srcs[y] %>% as.character() -> tmp2
    tmp2 <- ifelse(length(grep(",",tmp2)) == 0,as.numeric(tmp2),strsplit(tmp2,","))
    
    if(is.list(tmp2)){ # 複数
      tmp2 %>% unlist() %>% as.numeric() -> tmp2
      
      spout2 <<- lapply(1:length(tmp2),function(z){
        data2 <- tmp$morphs[tmp2[z]]
        system(paste("echo", data2 , "| cabocha -f1"), intern = T) -> out_text2
        gsub("^\\*.*","",out_text2) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text2
        spout2 <- lapply(1:length(out_text2),function(z){
          if(length(grep("助詞",out_text2[z])) != 0){
            strsplit(out_text2[z],",") %>% unlist() -> out_text3
            return(out_text3[1])
          }
        }) %>% unlist() %>% return()
      }) %>% unlist() %>% sort()
    }
    else{ # 一つだけ
      data2 <- tmp$morphs[tmp2]
      system(paste("echo", data2 , "| cabocha -f1"), intern = T) -> out_text2
      gsub("^\\*.*","",out_text2) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text2
      
      spout2 <- lapply(1:length(out_text2),function(z){
        if(length(grep("助詞",out_text2[z])) != 0){
          out_text2 <- strsplit(out_text2[z],",") %>% unlist()
          return(out_text2[1])
        }
      }) %>% unlist()
    }
    # 係り元の助詞がなければ終了
    if(is.null(spout2)) return()
    
    paste(spout,paste(spout2,collapse = " ")) %>% print() %>%   return()
  }) %>% unlist() %>% return()
}) %>% unlist()

out <- file("out.csv","w")
for(i in 1:length(spoil)){
  spoil[i] %>% strsplit(.," ") %>% unlist() -> ggg
  for(j in 1:length(ggg)){
    writeLines(ggg[j], out, sep = ",")
  }
  writeLines("", out, sep = "\n")
}
close(out)