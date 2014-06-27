corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        complete_cases<-complete(directory,)
        total_obs<-sum(complete_cases[,2])
        sulfate<-vector("numeric", length=total_obs)
        nitrate<-vector("numeric", length=total_obs)
       
        corr_vector<-vector("numeric", length=332)
        for(i in 1:332){
                corr_vector[i]<-NA
        }
        result_index<-0
        for (i in 1:332){
                
                if(complete_cases[i,2]>threshold){
                        if(i<10){
                                station_number_string<- paste("00",i,sep="")
                        } else if(i<100){
                                station_number_string<- paste("0",i,sep="")
                        } else {
                                station_number_string<- paste(i,sep="")
                        }
                        station_data<-read.csv(paste(directory,"/",station_number_string,".csv", sep=""))
                        for(k in 1:total_obs){
                                sulfate[k]<-NA
                                nitrate[k]<-NA
                        }
                        v_index<-0
                        for (j in 1:nrow(station_data)) {
                                if(!is.na(station_data[j,2])&!is.na(station_data[j,3])) {
                                        v_index=v_index+1
                                        sulfate[v_index]<-station_data[j,2]
                                        nitrate[v_index]<-station_data[j,3]
                        
                                }
                        }
                        result_index=result_index+1
                        corr_vector[result_index]<-cor(sulfate, nitrate, use="complete.obs")     
                }
        }
        
        head(corr_vector,result_index)
        
}