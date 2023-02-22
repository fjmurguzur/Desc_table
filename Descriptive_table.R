descriptive.table <- function(x,name=NA){

  y<-x[complete.cases(x)]
    
  result.table <- cbind.data.frame(Variable=name,Mean=round(mean(y),digits=1),Min=round(min(y),digits=1),Max=round(max(y),digits=1),
                             SD=round(sd(y),digits=1),CV=round(100*(sd(y)/mean(y)),digits=1),Total=length(y))
      

    write.table(result.table,"clipboard",sep="\t",row.names=F)
    return(result.table)
 }

