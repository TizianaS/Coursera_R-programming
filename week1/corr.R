corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        files <- list.files(directory, pattern="*.csv")  
        datacomplete<-complete(directory,1:332)
        idok<-datacomplete[,1][datacomplete[,2]>threshold]
        corre=numeric()
        i<-0
        setwd(directory)
        for (file in idok){
                i<-i+1
                dat<-na.omit(read.csv(files[file]))
                sulfates<-dat[,2]
                nitrates<-dat[,3]
                corre[i]<-cor(sulfates,nitrates)
        }
        setwd("..")
        corre
}