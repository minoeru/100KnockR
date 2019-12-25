Func19 <- function(){
    hoge <- read.table("hightemp.txt")
    fuga <- hoge[1]
    piyo <- unique(fuga)
    num <- nrow(piyo)
    count <- lapply(1:num,function(x){
        length( fuga[ fuga[1] == as.character(piyo[x,1]) ] )
    })
    count <- as.data.frame(unlist(count))
    ans <-  cbind(piyo,count)
    ans <- ans[order(ans[,2],decreasing=T),]
    print(ans)
}

Func19()
