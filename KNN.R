setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo 2")
load("titanic_train.Rdata")
library("dplyr")
library(ggplot2)
library(class)
library(foreach)
library(doParallel)
data <- titanic.train
data <- slice(data, -c(which(duplicated(data))))
data <- select(data, -Ticket, -Cabin)

a = makeCluster(7)
registerDoParallel(a)

set.seed(27)
#We need to convert all the factor variables into numeric values
#In this way knn can interpret them
data[,1] = as.numeric(data[,1])
data[,2] = as.numeric(data[,2])
data[,3] = as.numeric(data[,3])
data[,8] = as.numeric(data[,8])
for (i in 1:8){
  print(is.factor(data[,i]))
}


#We could try to normalize the data so it won´t be so problematic
#And so the output won´t be biased
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
data<- as.data.frame(lapply(data[,1:8], normalize))
#it doesn't work because we are also normalizing variables such as male or female

#Let´s try to do some hypertunning with knn incorporated
#We define a grid with values of k
#We sample the data, and create a train and test set
grid3 = expand.grid(k = 1:50)
grid3$accuracy = 0

dat.d <- sample(1:nrow(data),size=nrow(data)*0.7,replace = FALSE)
train.1 <-data[dat.d, -1] 
test.1 <- data[-dat.d, -1]
#now we create the set2 for the conclusions (with the label, the "factor of true classification from the set")
train.2 <- data[dat.d, 1]
test.2 <-data[-dat.d, 1]
#We develope the loop for testing
for (i in 1:nrow(grid3)){
  knn.26 <- knn(train=train.1, test=test.1, cl=train.2, k=i)
  ACC.26 <- sum(test.2 == knn.26)/NROW(test.2)
  grid3$accuracy[i] = grid3$accuracy[i] + ACC.26
}
acc = grid3$accuracy[which.max(grid3$accuracy)]

