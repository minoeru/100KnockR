library("magrittr")
Func2 <- function(tex1,tex2){
  strsplit(tex1,"") %>% unlist() -> tex1
  strsplit(tex2,"") %>% unlist() %>% paste0(tex1,.,collapse = "") %>% return()
}

Func2("パトカー","タクシー")