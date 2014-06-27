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
                                        
                                        sulfate=sulfate+station_data[j,2]
                                        s_obs<-s_obs+1
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