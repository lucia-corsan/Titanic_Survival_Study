setwd("C:/Users/Alonso/Desktop/00.ALONSO/Ciencia de datos/Trabajo 2")
load("titanic_train.Rdata")
data <- titanic.train
View(data)
library(rpart)
library(rpart.plot)
#first of all we will start eliminating duplicates and misvalues

#As the variable ticket is a number that was randomly created we eliminate 
#it from the data frame

#As the cabin variable is incomplete we will eliminate it from the data
library("dplyr")
data <- slice(data, -c(which(duplicated(data))))
data <- select(data, -Ticket, -Cabin)

set.seed(27)

grid = expand.grid(minsplit = seq(10,60,10),
                   cp = 2^(0:-10),
                   maxdepth = seq(1, 10, 1))


n = nrow(data)



n_fold = 5; grid$accuracy = 0
folds = sample(rep(1:n_fold, length.out = n))
for (j in 1:n_fold){
  # split
  train = data[folds != j,]; test = data[folds == j,]
  for (i in 1:nrow(grid)){
    # fit and evaluate
    tree = rpart(Survived~., train,
                 control = rpart.control(minsplit = grid$minsplit[i],
                                         maxdepth = grid$maxdepth[i],
                                         cp = grid$cp[i]))
    pred = predict(tree, test, type = "class")
    acc = (sum(pred == test$Survived)/length(pred))
    # average
    grid$accuracy[i] = grid$accuracy[i] + acc/n_fold
  }}
maxacc = grid[which.max(grid$accuracy),]
