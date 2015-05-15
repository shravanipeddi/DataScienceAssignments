pollutantmean <- function(directory,pollutant,id=1:332){
    firstCount <- id[1]
    maxValueOfI <- max(id)
    for (i in id){
        lengthOfI <- nchar(as.character(i));
        lengthOfMaxI <- nchar(as.character(maxValueOfI))
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
        print(fileName)
        if(firstCount == id[1]){
            pollutantTable <- read.csv(fileName,header=TRUE)
        }
        else{
            pollutantNewTable <- read.csv(fileName,header=TRUE)
            pollutantTable <- rbind(pollutantTable,pollutantNewTable)
        }
        firstCount = firstCount+1
    }
    #print(pollutantTable)
    pollutantData <- pollutantTable[pollutant]
    #print(pollutantData)
    naValues <- is.na(pollutantData)
    #print(naValues)
    withoutNA <- pollutantData[!naValues]
    #print(withoutNA)
    mean(withoutNA)

}