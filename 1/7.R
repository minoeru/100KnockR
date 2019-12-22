args1 = commandArgs(trailingOnly=TRUE)[1]
args2 = commandArgs(trailingOnly=TRUE)[2]
args3 = commandArgs(trailingOnly=TRUE)[3]

Func7 <- function(x,y,z){
  return(paste0(x,"時の",y,"は",z))
}

Func7(args1,args2,args3)
