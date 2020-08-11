library("magrittr")
Func1 <- function(text){
	text %>% strsplit(.,"") %>% unlist() %>% .[c(1,3,5,7)] %>% paste(.,collapse = "") %>% return()
}

Func1("パタトクカシーー")