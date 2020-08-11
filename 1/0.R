library("magrittr")
Reverse <- function(tex){
    tex %>% strsplit(.,"") %>% unlist() %>% rev() %>% paste(.,collapse = "") %>% return()
}

Reverse("stressed")