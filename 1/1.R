Func1 <- function(text){
	num <- c(1,3,5,7)
	hoge <- lapply(1:length(num),function(x){substring(text,num[x],num[x])})
	return(paste(unlist(hoge),collapse=""))

}
Func1("パタトクカシーー")
