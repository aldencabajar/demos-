library(dplyr)
library(xlsx)

file <- loadWorkbook("/Users/mac/Desktop/Delloite questionnaire data tabulation.xlsx")
file.string <- "/Users/mac/Desktop/BA thesis/Delloite questionnaire data tabulation.xlsx"


branches <- c("itpark","btc","ayala" ,"mandaue","sm")
dir <- "/Users/mac/Desktop/BA thesis/"

n.respond <- 50
n.ques <- 19
respondent <- data.frame(respondents= c(1:n.respond), row.names=NULL)

sheet<- sapply(6:n.ques,function(x) createSheet(file, sheetName = paste("Q",x, sep="")))
sapply(1:length(6:n.ques), function(x) addDataFrame(respondent,sheet[[x]], row.names=F ))


paste.dbl <- function(str1, var) {
  paste( 
    paste(str1,var, sep= ""), "xlsx", sep="."
  )
}


branch.files <- sapply(branches, function(x) loadWorkbook(paste.dbl(dir, x)))
branch.sheets <- sapply(branch.files, getSheets)  
main.sheets <- getSheets(file)

####Formatting of column headers####
al <- Alignment(wrapText=T,v="VERTICAL_CENTER")
cs <- CellStyle(file, alignment =al)

branch.data <- list()
data.summr <- list()

for(i in 1:length(branches))
{
  tmp <-list()
  for(j in 6:8+1)
  {
    res <- readColumns(branch.sheets[[i]][[j]],1,20,3,endRow=15)
    tmp[[j]] <- res
    branch.data[[i]] <- tmp
    
    headers <- colnames(branch.data[[i]][[j]])
    
    if (length(grep("\\X.", headers)) > 0)
    {
      lim <- min(grep("\\X.", headers))-1
    }
    else {
      lim <- length(headers)
    }
    
    colnames(branch.data[[i]][[j]]) <- gsub("\\.", " ",headers)

    
    if (i == 1)
    {
      addDataFrame(branch.data[[i]][[j]][1:10,2:lim], 
                   main.sheets[[j]], row.names=F, col.names=T, 
                   startRow=10*i-9,startColumn=2,colnamesStyle=cs)
    }  
      else {
        addDataFrame(branch.data[[i]][[j]][1:10, 2:lim], 
                     main.sheets[[j]], row.names=F, col.names=F, 
                     startRow=10*i-8,startColumn=2, colnamesStyle=cs)
      }
    
  setColumnWidth(main.sheets[[j]], 1:15, 15)
  
  data.summr[[j]] <- readColumns(main.sheets[[j]],1,lim,1)
  
  
    
  }  
}



####Processing of data for questions with numerical values#####


fnc <- function(data){
        op <- list()
        wt.mean <- c()
        for (col in 2:ncol(data))
        {
          op[[col-1]] <- data.frame(table(data[,col]))
          
          op[[col-1]]$Var1 <- as.numeric(levels(op[[col-1]]$Var1))
          
          freqprc <- sapply(1:nrow(op[[col-1]]), function(x) op[[col-1]]$Freq[x]/sum(op[[col-1]]$Freq)*100 )
          
          op[[col-1]] <- cbind(op[[col-1]], freqprc)
          
          colnames(op[[col-1]]) <- c("Var", "Freq", "%Freq")
          
          wt.mean[col-1] <- sum(op[[col-1]]$Var*op[[col-1]]$Freq)/nrow(data)
          
          
        }
        return (list(summr=op, wtd.mean=wt.mean))
}             
procfnc <- function(data.summr, n.ques){
            n.data <- n.ques+1
            qsumr <- fnc(data.summr[[n.data]])$summr
            qwmean <- fnc(data.summr[[n.data]])$wtd.mean
            qcol <- colnames(data.summr[[n.data]])
            sapply(1:length(qsumr), function(x) addDataFrame(qsumr[[x]],main.sheets[[n.data]], startRow=56, startColumn=4*x-3, row.names=F))
            sapply(1:length(qsumr), function(x) addDataFrame(paste("Entry",x, sep=" "), main.sheets[[n.data]],startRow=54, startColumn=4*x-3, rownamesStyle=cs, row.names=F, col.names=F ))
            sapply(1:length(qsumr), function(x) addDataFrame(t(data.frame(wtd.mean=qwmean[[x]])),main.sheets[[n.data]], startRow=57+nrow(fnc(data.summr[[n.data]])$summr[[x]]), startColumn=4*x-3, row.names=T, col.names=F ))
            addDataFrame(data.frame(entry=gsub("\\.", " ",qcol[2:length(qcol)]), wtd.mean=qwmean), main.sheets[[n.data]], startRow=75, startColumn=1, row.names=F)
}

procfnc(data.summr,6)
procfnc(data.summr,7)
procfnc(data.summr,12)


procfnc2 <- function(dat, n.ques){
  n.ques <- n.ques+1
  tmp <- list()
  for (n in 2:ncol(dat[[n.ques]]))
  {
    col.dat <- dat[[n.ques]][n] #### column data
    unq <- unique(col.dat)   ##### unique data in each column
    if (length(levels(unq[,1])) > 2)
    {
      freq <- sapply(1:length(unq[,1]), function(x) length(grep(paste0("^",unq[,1][x],"$"), col.dat[,1])))
      tmp[[n-1]] <- cbind(unq,freq, freqprc=freq/sum(freq)*100 )
      
    }
    else{
      tmp[n-1] <- length(grep(paste(unq[,1][2]), col.dat[,1], fixed=T ))
      
    }  
  }
   if (class(tmp[[1]]) == "integer")
   {
     fnl <- data.frame(tmp, row.names="count")
     colnames(fnl) <- colnames(dat[[n.ques]][2:ncol(dat[[n.ques]])])
     fnl <- t(rbind(fnl, freqprc = fnl/sum(fnl)*100))
     row.names(fnl) <- gsub("\\."," ", row.names(fnl) )
     addDataFrame(fnl, main.sheets[[n.ques]], startRow=55, startColumn=1)
     
   }
  else{
    sapply(1:length(tmp), function(x) addDataFrame(tmp[[x]], main.sheets[[n.ques]], startRow=55, startColumn=4*x-3, row.names=F) )
  }
  
}

ques <- c(8:11,13:19)

sapply(1:length(ques), function(x) procfnc2(data.summr,ques[x] ))

saveWorkbook(file, file.string)


