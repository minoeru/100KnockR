

Func2("パトカー","タクシー")

Func2 <- function(tex1,tex2){
	num <- nchar(tex1)
	hoge <- lapply(1:num,function(x){paste0(substring(tex1,x,x),substring(tex2,x,x))})
	return(paste(unlist(hoge),collapse=""))
}
