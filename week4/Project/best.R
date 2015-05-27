##load package for matrix opearations (such as finding the min in a column)
library("matrixStats")
##Unzip data
unzip("./rprog-data-ProgAssignment3-data.zip", exdir = "./data/")
##create a function called best taking two arguments: the 2-character abbreviated name of a state 
##and an outcome name. The function reads the outcome-of-care-measures.csv file and returns 
##a character vector with the name of the hospital that has the best (i.e. lowest) 
##30-day mortality for the specified outcome in that state
best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character",na.string="Not Available")
        ## Check that state and outcome are valid, meaning belong to the list of state an are  either heart attack, 
        ## heart failure or pneumonia respectively
        state_pos<-unique(data$State)
        outcome_pos<-c("heart attack","heart failure","pneumonia")
        if(!is.element(state,state_pos)){
                stop("invalid state")
        }
        if(!is.element(outcome,outcome_pos)){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        else {
                if (outcome == "heart attack"){
                        datastate<-subset(data,State== state,select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
                }
                if (outcome == "heart failure"){
                        datastate<-subset(data,State== state,select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
                }
                if (outcome == "pneumonia"){
                        datastate<-subset(data,State== state,select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
                }   
              
                index<-which(as.numeric(datastate[,2])==min(as.numeric(datastate[,2]),na.rm=TRUE))
                if (length(index)==1){
                        ans<- datastate$Hospital.Name[index]       
                }
                else {
                        vec<-sort(datastate$Hospital.Name[index])
                        ans<- vec[1]
                }
        ans
                
        }
}