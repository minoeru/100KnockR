Reverse <- function(tex){
    num <- nchar(tex)
    hoge <-lapply(1:num,function(x){substring(tex,num-x+1,num-x+1)})
    return (paste(unlist(hoge),collapse=""))
}

Reverse("stressed")
