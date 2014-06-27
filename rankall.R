## rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num).
## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specied in num. For example 
## the function call rankall("heart attack", "best") would return a data frame containing the
## names of the hospitals that are the best in their respective states for 30-day heart attack
## death rates. The function should return a value for every state (some may be NA).
## The first column in the data frame is named hospital, which contains the hospital name, 
## and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded
## from the set of hospitals when deciding the rankings. The rankall function should handle
## ties in the 30-day mortality rates in the same way that the rankhospital function handles ties.

rankall <- function(outcome, num = "best") {
        Rprof()
        ## Read outcome data
        df <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        valid_outcomes_cap <- c("Heart.Attack","Heart.Failure","Pneumonia")
        if(is.na(match(outcome, valid_outcomes))) {
                stop("invalid outcome")
        } else {
                disease <-match(outcome, valid_outcomes)
                col_string <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",valid_outcomes_cap[disease], sep="")
        }
        if(!is.numeric(num)) {
                if(!(num=="best"|num=="worst")){
                        stop("invalid number")
                }
        } else if(!num%%1==0){
                stop("invalid number")
        }
        ## handle num equal to best i.e. number 1 (worst will be handled later as it varies)
        if (num=="best"){
                nnum<-1
        }
        
        ## build a complete state abbreviation vector based on state.abb and 4 additional "states"
        ## DC, GU, PR,VI
        states <- c(state.abb[1:50],"DC","GU","PR","VI")
        states <- sort(states)
        
        ## get the outcome stats and convert the outcome col to numeric from 'factor'
        suppressWarnings(df[,col_string] <- as.numeric(as.character(df[,col_string])))
        
        rdf <- data.frame(hospital=rep(NA,54),state=rep(NA,54))
        ## For each state, find the hospital of the given rank
        for (i in seq_along(states)){
                df[] <- df[order(df[,"State"],df[,col_string],df[,"Hospital.Name"]),]
                xx<-df[df[,"State"]==states[i],]
                rdf[i,2]<-states[i]
                if(num=="worst"){
                        nnum<-length(xx[!is.na(xx[,col_string]),col_string])
                } else if(!num=="best"){
                        nnum<-num
                }
                
                if(nnum<=nrow(xx)&!is.na(xx[nnum, col_string])){
                        rdf[i,1]<-xx[nnum,"Hospital.Name"]
                }
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        rdf
        Rprof(NULL)

}

