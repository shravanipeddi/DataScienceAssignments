corr <- function(directory,threshold = 0){
    all <- complete(directory,1:332)
    valids <- all$nobs > threshold
    valid <- all[valids,]
    countOfRows <- nrow(valid)
    count = 1
    if( countOfRows > 0){
        for (i in valid$id){
            lengthOfI <- nchar(as.character(i));
             if(lengthOfI == 1){
                #print(diffLength)
                trueI <- paste(c("00",i),collapse='')
            }
            else if(lengthOfI == 2){
                trueI <- paste(c("0",i),collapse='')
            }
            else{
                trueI <- i
            }
            fileName <- paste(c(directory,"/",trueI,".csv"),collapse='')
            pollutantTable <- read.csv(fileName,header=TRUE)
            pollutantTable <- pollutantTable[complete.cases(pollutantTable),]
            if(count == 1){
                correlation <- cor(pollutantTable$sulfate,pollutantTable$nitrate)    
            }
            else{
                correlation <- c(correlation,cor(pollutantTable$sulfate,pollutantTable$nitrate))
            }
            count = count + 1;
        }
        correlation
    }
    else{
        as.numeric(NULL)
    }
}