complete <- function(directory,id=1:332){
    #maxValueOfI <- max(id)
    count <- 1
    for (i in id){
        lengthOfI <- nchar(as.character(i))
        #lengthOfMaxI <- nchar(as.character(maxValueOfI))
        #diffLength <- lengthOfMaxI-lengthOfI

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
        #print(fileName)

        pollutantTable <- data.table(read.csv(fileName,header=TRUE))
		row <- nrow(pollutantTable[complete.cases(pollutantTable),])
		if(count==1){
			b <- list(i,row)
		}
		else{
			b <- rbind(b,list(i,row))			
		}
		count <- count + 1
    }
    b <- data.frame(b)
    colnames(b) <- c("id","nobs")
    b
}