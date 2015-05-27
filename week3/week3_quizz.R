library(datasets)
data(iris)
data<-split(iris,iris$Species)$virginica
data<-as.data.frame(data)
ans<-mean(data$Sepal.Length)

r<-apply(iris[, 1:4], 2, mean)
r

library(datasets)
data(mtcars)
ans2<-with(mtcars, tapply(mpg, cyl, mean))
ans3<-with(mtcars, tapply(hp, cyl, mean))

debug(ls)
ls