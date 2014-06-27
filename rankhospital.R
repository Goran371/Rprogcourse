## The function rankhospital() takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that
## state for that outcome (num).
## The function reads the outcome-of-care-measures.csv le and returns a character 
## vector with the name of the hospital that has the ranking specied by the num argument.
##
## The num argument can take values \best", \worst", or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.

## If multiple hospitals have the same 30-day mortality rate for a given cause
## of death ties should be broken by using the hospital name (alphabetically).

rankhospital <- function(state, outcome, num = "best") {
        ## validate arguments
        if(is.na(match(state, state.abb))) {
                stop("invalid state")
        }
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
        
        
        ## Read outcome data
        outcome_d <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        
        ## filter out the data for one state only
        outcome_d_state <- outcome_d[(outcome_d[,"State"]==state),]
        
        ## get the outcome stats and convert the outcome col to numeric from 'factor'
        suppressWarnings(outcome_d_state[,col_string] <- as.numeric(as.character(outcome_d_state[,col_string])))
        
        ## sort with lowest mortality first            
        sorted <- outcome_d_state[order(outcome_d_state[,col_string],outcome_d_state[,"Hospital.Name"]),]
        
        ## drop all NA lines at the end
        sorted_no_NA <- sorted[!is.na((sorted[,col_string])),]
        
        ## Return hospital name in that state with the given rank 30-day death rate
        if(num=="best") {
                num=1
        } else if(num=="worst") {
                num=nrow(sorted_no_NA)
        }
        sorted_no_NA[num,"Hospital.Name"]
        
}

