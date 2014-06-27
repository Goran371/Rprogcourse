## Write a function called best that take two arguments: the 2-character abbreviated name
## of a state and an outcome name. The function reads the outcome-of-care-measures.csv ?le
## and returns a character vector with the name of the hospital that has the best (i.e. lowest)
## 30-day mortality for the specified outcome in that state. The hospital name is the name
## provided in the Hospital.Name variable. The outcomes can be one of "heart attack", 
## "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.
## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital
## names should be sorted in alphabetical order and the first hospital in that set should be
## chosen (i.e. if hospitals "b", "c",and "f" are tied for best, then hospital "b" should 
## be returned).

best <- function(state, outcome) {
        ## validate arguments
        if(is.na(match(state, state.abb))) {
                stop("invalid state")
        }
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        valid_outcomes_cap <- c("Heart.Attack","Heart.Failure","Pneumonia")
        if(is.na(match(outcome, valid_outcomes))) {
                stop("invalid outcome")
        } else {
                discease <-match(outcome, valid_outcomes)
                col_string <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",valid_outcomes_cap[discease], sep="")
        }
        ## Read outcome data
        outcome_d <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        ## filter out the data for one state only
        outcome_d_state <- outcome_d[(outcome_d[,"State"]==state),]
        ## get the outcome stats
        suppressWarnings(outcome_data<- as.numeric(outcome_d_state[,col_string]))
        
        best_outcome <- min(outcome_data,na.rm=TRUE)
        best <- match(best_outcome, outcome_data)
        winner <- outcome_d_state[best,"Hospital.Name"]
        ## there may be more than one winner
        j<-0
        winner_list<-winner
        for (i in 1:nrow(outcome_d_state)) {
                if(outcome_d_state[i,col_string]==best_outcome){
                        j= j+1
                        winner_list[j] <-outcome_d_state[i, "Hospital.Name"]     
                }
                
        }
        winner_list <- sort(winner_list)
        
        ## Return hospital name in that state with lowest 30-day death rate
        winner_list[1]
}

## The function should check the validity of its arguments. If an invalid state value is passed
## to best, the function should throw an error via the stop function with the exact message
## "invalid state". If an invalid outcome value is passed to best, the function should throw
## an error via the stop function with the exact message "invalid outcome".
