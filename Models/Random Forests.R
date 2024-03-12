setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo 2")
load("titanic_train.Rdata")
data <- titanic.train
View(data)
library("dplyr")
library(ggplot2)
library(randomForest)
library(foreach)
library(doParallel)

#first of all we will start eliminating duplicates and missvalues
data <- slice(data, -c(which(duplicated(data))))


#As the variable ticket is a number that was randomly created we eliminate 
#it from the data frame

#As the cabin variable is incomplete we will eliminate it from the data
data <- select(data, -Ticket, -Cabin)

idx_train = sample(1:nrow(data), floor(0.75*nrow(data)))
train = data[idx_train,]
test = data[-idx_train,]

#Random Forest
RF = randomForest(Survived~., train, keep.forest = TRUE)
#keep forest: to make a classification model instead of a regression one.
pred = predict(RF, test)
accuracy = (sum(pred == test$Survived)/length(pred)); accuracy


set.seed(27)

grid2 = expand.grid(mtry = seq(2,7,1),
                    ntree = seq(500, 3500, 100))
n2 = nrow(grid2)
#now implementing parallell processing so my computer wont explode

cl = makeCluster(7)
registerDoParallel(cl)

accuracy2 = foreach(i = 1:n2, .combine = "c", .packages = "randomForest")%dopar%{
  RF2 = randomForest(Survived~., train,
                    mtry = grid2$mtry[i],
                    ntree = grid2$ntree[i])
  pred2 = predict(RF2, test, type = "class")
  acc = (sum(pred2 == test$Survived)/length(pred2))
}

library(ggthemes)
ggplot(grid2)+aes(x = ntree, y = accuracy2, color = as.factor(mtry))+
  geom_point(size = 0.5) + geom_line(size = 1) +
  #theme(text = element_text(size = 5))
  theme_hc(bgcolor = "darkunica")

max(accuracy2)
