## Write a function that reads a directory full of files and reports the number of completely 
## observed cases in each data file. The function should return a data frame where the first
## column is the name of the file and the second column is the number of complete cases.
## A prototype of this function follows
complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        complete_cases <-data.frame("id"=id,"nobs"=rep(0,length(id)))
        row<-0
        for (i in id){
                row=row+1
                
                if(i<10){
                        station_number_string<- paste("00",i,sep="")
                } else if(i<100){
                        station_number_string<- paste("0",i,sep="")
                } else {
                        station_number_string<- paste(i,sep="")
                }
                station_data<-read.csv(paste(directory,"/",station_number_string,".csv", sep=""))
                
                for (j in 1:nrow(station_data)) {
                        if(!is.na(station_data[j,2])&!is.na(station_data[j,3])) {
                                
                                complete_cases[row,2]<-complete_cases[row,2]+1
                        }
                }
                
        }
        complete_cases
        
}