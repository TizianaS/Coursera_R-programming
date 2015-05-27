##load package for matrix opearations (such as finding the min in a column)
library("matrixStats")
##Unzip data
unzip("./rprog-data-ProgAssignment3-data.zip", exdir = "./data/")
##create a function called best taking two arguments: the 2-character abbreviated name of a state 
##and an outcome name. The function reads the outcome-of-care-measures.csv file and returns 
##a character vector with the name of the hospital that has the best (i.e. lowest) 
##30-day mortality for the specified outcome in that state
rankhospital <- function(state, outcome, num = "best") {
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
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        else {
                if (outcome == "heart attack"){
                        outcomestring<- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                        datastate<-subset(data,State== state,select=c("Hospital.Name",outcomestring))
                }
                if (outcome == "heart failure"){
                        outcomestring<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                        datastate<-subset(data,State== state,select=c("Hospital.Name",outcomestring))
                }
                if (outcome == "pneumonia"){
                        outcomestring<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                        datastate<-subset(data,State== state,select=c("Hospital.Name",outcomestring))
                }   
                
                ordereddata<-datastate[order(as.numeric(datastate[,2]),datastate["Hospital.Name"],na.last=NA),]
                n<-length(ordereddata[,2])
                if (num=="best"){
                        num<-1
                }
                if (num=="worst"){
                        num<-n
                }
                ordereddata<-cbind(ordereddata,Rank=1:n)
                rankhosp<-ordereddata$Hospital.Name[ordereddata$Rank==num]
                ans<-rankhosp[1]              
        }
        ans
}