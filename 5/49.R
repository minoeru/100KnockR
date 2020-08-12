library("magrittr")
# hoge <- scan("neko.txt",what = character(),sep = "\n",blank.lines.skip = T)
# hoge <- gsub("^　","",hoge) %>% gsub("。|、","",.)

MakeDependency <- function(n){
  # input <- hoge[n]
  input <- "吾輩はここで始めて人間というものを見た"
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

UseCabo <- function(text){
  system(paste("echo", text , "| cabocha -f1"), intern = T) %>%  gsub("^\\*.*","",.) %>% gsub("EOS","",.) %>% gsub("\t",",",.) %>% .[-which(. %in% "")] %>%  return()
}

tmp <- MakeDependency(1)
sub_flag <- 0

for(j in 1:length(tmp$morphs)){
  out_text <- UseCabo(tmp$morphs[j])

  if(length(grep("[^代]名詞",out_text)) != 0){
    # 主語 -> 名詞 + 係助詞
    for(hoge in 1:length(out_text)){
      out_tmp <- strsplit(out_text[hoge],",") %>% unlist()
      if(length(grep("係助詞",out_tmp)) != 0) {
        subject <<- paste0("X",out_tmp[1]," | ")
        sub_flag <- 1
      }
      else if(length(grep("助詞",out_tmp)) != 0){
        subject <<- paste0("X",out_tmp[1])
        sub_flag <- 2
      }
    }
    
    if(sub_flag == 1){ # 主語がある
      for(fuga in (j+1):length(tmp$morphs)){
        ans <<- subject
        number <- fuga
        Y_flag <- 0 # Yに変換するフラグ
        
        while(TRUE){
          if(tmp$dst[number] == -1){
            if(ans == subject) break
            paste(ans," | ", tmp$morphs[tmp$dst == -1]) %>% print() #述語追加
            break
          }
          if(Y_flag == 0){
            y_data <- UseCabo(tmp$morphs[number])
            if(length(grep("[^代]名詞",y_data)) != 0){
              for(piyo in 1:length(y_data)){
                out_tmp <- strsplit(y_data[length(y_data) - 1 - piyo],",") %>% unlist()
                if(length(grep("助詞[^類]?",out_tmp)) != 0) {
                  ans <<- paste0(ans,"Y",out_tmp[1])
                  Y_flag <<- 1
                }
              }
            }
            else break
          }
          else ans <<- paste(ans," -> ",tmp$morphs[number])
          number <- tmp$dst[number]
        }
      }
    }
    
    else if(sub_flag == 2){ # 主語がない
      for(fuga in (j+1):length(tmp$morphs)){
        number <- fuga
        ans <<- subject
        while(TRUE){
          if(tmp$dst[number] == -1) break
          out_text <- UseCabo(tmp$morphs[number])
          if(length(grep("[^代]名詞",out_text)) != 0){
            paste0(ans," -> Y") %>% print()
            
            if(tmp$dst[tmp$dst[number]] != -1){
              while(TRUE){
                tmp_ans <- paste0(ans," -> ",tmp$morphs[number])
                out_text <- UseCabo(tmp$morphs[tmp$dst[number]])
                if(length(grep("[^代]名詞",out_text))) paste0(tmp_ans," -> Y") %>%  print()
                number <<- tmp$dst[number]
                if(tmp$dst[tmp$dst[number]] == -1) break
              }
            }
            break
          }
          else ans <<- paste0(ans," -> ",tmp$morphs[number])
          number <<- tmp$dst[number]
        }
        if(ans != subject) break
      }
    }
  }
}