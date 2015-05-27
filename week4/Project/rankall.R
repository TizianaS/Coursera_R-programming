##load package for matrix opearations (such as finding the min in a column)
library("matrixStats")
##Unzip data
unzip("./rprog-data-ProgAssignment3-data.zip", exdir = "./data/")
##Function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
##(num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
##containing the hospital in each state that has the ranking specified in num. 
rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character",na.string="Not Available")
        ## Check that outcome is valid, meaning belong to the list of heart attack, 
        ## heart failure or pneumonia 
        state_pos<-sort(unique(data$State))
        outcome_pos<-c("heart attack","heart failure","pneumonia")
        ans<-character()
        if(!is.element(outcome,outcome_pos)){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        else {
               for (state in state_pos)
                       {
                       
                        if (outcome == "heart attack")
                                {
                                        outcomestring<- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                                        datastate<-subset(data,State== state,select=c("Hospital.Name",outcomestring))
                                  }
                         if (outcome == "heart failure")
                                 {    
                                        outcomestring<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                                        datastate<-subset(data,State== state,select=c("Hospital.Name",outcomestring))
                                 }
                        if (outcome == "pneumonia")
                                {
                                         outcomestring<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                                         datastate<-subset(data,State== state,select=c("Hospital.Name",outcomestring))
                                 }   
                
                          ordereddata<-datastate[order(as.numeric(datastate[,2]),datastate["Hospital.Name"],na.last=NA),]
                          n<-length(ordereddata[,2])
                         if (num=="best"){
                                  i<-1
                                 }
                          else if (num=="worst"){
                                i<-nrow(ordereddata)
                                }
                          else {
                                  i<-num
                          }                        
                ordereddata<-cbind(ordereddata,Rank=1:n)
                rankhosp<-ordereddata$Hospital.Name[ordereddata$Rank==i]
                ans <-cbind(ans,rankhosp[1])       
                
               }
                
        }
        ans<-as.vector(ans)
        result<-as.data.frame(cbind(hospital=ans,state=state_pos))
        write.table(result, file = "rankall.txt",row.name=FALSE)
        result
}