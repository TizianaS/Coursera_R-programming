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
        files <- list.files(directory, pattern="*.csv")
                 
        vecncomp<-numeric(length(id))
        
        vecid<-numeric(length(id))

        setwd(directory)
        i<-0
        for (file in id){
                i<-i+1
                data<-read.csv(files[file])
                iscomp<-complete.cases(data)
                ncomp<-sum(iscomp>0)
                vecncomp[i]<-ncomp
                vecid[i]<-file  
        }
dataframe<-cbind.data.frame(id=vecid,nobs=vecncomp)
setwd("..")
dataframe
}