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

spout <- lapply(1:10,function(x){
  tmp <- MakeDependency(x)
  
  spoil <- lapply(1:length(tmp$morphs),function(y){
    # 原型の抽出
    data1 <- tmp$morphs[y]
    system(paste("echo", data1 , "| cabocha -f1"), intern = T) %>%  gsub("^\\*.*","",.) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text
    verb <- lapply(1:length(out_text),function(z){
      if(length(grep("[^助容]動詞",out_text[z])) != 0){
        out_text <- strsplit(out_text[z],",") %>% unlist()
        return(out_text[8])
      }
    }) %>% unlist()
    
    # 動詞がなければ終了
    if(is.null(verb)) return()
    #係り元がなければ終了
    if(tmp$srcs[y] %>% as.character() == "") return()
    
    # 係り元
    tmp$srcs[y] %>% as.character() -> tmp2
    tmp2 <- ifelse(length(grep(",",tmp2)) == 0,as.numeric(tmp2),strsplit(tmp2,","))
    
    if(is.list(tmp2)){ # 複数
      tmp2 %>% unlist() %>% as.numeric() -> tmp2
      flag <<- numeric(length(tmp2)) * NA
      particle <<- lapply(1:length(tmp2),function(z){
        data2 <- tmp$morphs[tmp2[z]]
        system(paste("echo", data2 , "| cabocha -f1"), intern = T) %>% gsub("^\\*.*","",.) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text2
        particle <- lapply(1:length(out_text2),function(n){
          if(length(grep("助詞[^類]",out_text2[n])) != 0){
            flag[z] <<- tmp2[z]
            strsplit(out_text2[n],",") %>% unlist() -> out_text3
            return(out_text3[1])
          }
        }) %>% unlist() %>% .[length(.)] %>% return()
      }) %>% unlist()
      
      # 係り元の助詞がなければ終了
      if(is.null(particle)) return()
      # 50音順にソート
      flag <- flag[!is.na(flag)]
      flag <- flag[order(particle)]
      particle <- sort(particle)
      ans <<- paste(tmp$morphs[flag])
    }
    else{ # 一つだけ
      data2 <- tmp$morphs[tmp2]
      system(paste("echo", data2 , "| cabocha -f1"), intern = T) %>% gsub("^\\*.*","",.) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] -> out_text2
      particle <- lapply(1:length(out_text2),function(z){
        if(length(grep("助詞",out_text2[z])) != 0){
          out_text2 <- strsplit(out_text2[z],",") %>% unlist()
          return(out_text2[1])
        }
      }) %>% unlist()
      
      # 係り元の助詞がなければ終了
      ifelse(is.null(particle),return(),ans <<- tmp$morphs[tmp2])
    }
    paste(verb,paste(particle,collapse = " "),paste(ans,collapse = " ")) %>% print() %>% return()
  }) %>% unlist() %>% return()
}) %>% unlist()

out <- file("out2.csv","w")
for(i in 1:length(spoil)){
  spoil[i] %>% strsplit(.," ") %>% unlist() -> fuga
  for(j in 1:length(fuga)){
    writeLines(fuga[j], out, sep = ",")
  }
  writeLines("", out, sep = "\n")
}
close(out)