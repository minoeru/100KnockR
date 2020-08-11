library("magrittr")

Change <- function(tex){
  top <- substring(tex,1,1)
  end <- substring(tex,nchar(tex),nchar(tex))
  center <- substring(tex,2,nchar(tex)-1) %>% strsplit(.,"") %>% unlist()
  floor(runif(1000,1,length(center)+1)) %>% unique() %>% center[.] %>% paste(.,collapse = "") %>% paste0(top,.,end,collapse = "") %>% return()
}

Func9 <- function(tex){
  hoge <- unlist(strsplit(tex," "))
  lapply(1:length(hoge),function(x){ifelse(nchar(hoge[x]) < 4,hoge[x],Change(hoge[x]))}) %>% unlist() %>% paste(.,collapse = " ") %>% return()
}

Func9("I couldn’t believe that I could actually understand what I was reading : the phenomenal power of the human mind.")