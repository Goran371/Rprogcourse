f<-function(x,y){
        x^2+y/z
}

make.power <- function(n) {
        pow <- function(x) {
                x^n 
        }
        pow 
}
f1<-function(y){
        if(y>5){
                y<-0
        }
}
f2 <-function(x){
        g<- function(y){
                y+z
        }
        z<- 4
        x+g(x)
}
y <- 10
f <- function(x) {
        y <- 2
        y^2 + g(x)
}
g <- function(x) { 
        x*y
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        s_obs <-0
        n_obs <-0
        sulfate <-0
        nitrate <-0
        for (i in id){
                print(i)
                print (id)
                
                if(i<10){
                        station_number_string<- paste("00",i,sep="")
                } else if(i<100){
                        station_number_string<- paste("0",i,sep="")
                } else {
                        station_number_string<- paste(i,sep="")
                }
                station_data<-read.csv(paste(directory,"/",station_number_string,".csv", sep=""))
                
                for (j in 1:nrow(station_data)) {
                        
                      if(pollutant=="sulfate"){
                              if(!is.na(station_data[j,2])){
                                      print(j)
                                      sulfate=sulfate+station_data[j,2]
                                      print(s_obs <- s_obs+1)
                                      print(sulfate)
                                      print(s_obs)
                              }
                      } else if(!is.na(station_data[j,3])){
                              nitrate=nitrate+station_data[j,3]
                              n_obs<-n_obs+1
                      }
                }
        }
        if(pollutant=="sulfate"){
                
                sulfate/s_obs
                
        } else if (pollutant=="nitrate") {
                nitrate/n_obs
        }
}

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
        for (i in id){
                                
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
                                print(station_data[j,2])
                                print(station_data[j,3])
                                complete_cases[i,2]<-complete_cases[i,2]+1
                                print(complete_cases[i,2])
                        }
                }
                        
        }
        print(complete_cases)
        complete_cases

}

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        complete_cases<-complete(directory,)
        for (i in 1:332){
                
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
                                sulfate[(station_data[j,2])
                                print(station_data[j,3])
                                complete_cases[i,2]<-complete_cases[i,2]+1
                        }
                }
                
        }
        
        head(comp)
        
        
}

