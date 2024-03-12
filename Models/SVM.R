#support vector machine SVM
setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo 2")
load("titanic_train.Rdata")
data <- titanic.train
library("dplyr")
library(ggplot2)
library(e1071)

data <- slice(data, -c(which(duplicated(data))))
data <- select(data, -Ticket, -Cabin)

n = nrow(data)

set.seed(27)
#As always we create the two main sets
idx_train = sample(1:nrow(data), floor(0.75*nrow(data)))
train = data[idx_train,]
test = data[-idx_train,]

#Polynomial kernel
#First of all let´s create our grid of the variable degree
grid4 = expand.grid(degree = 1:100)
#in this particular case we´re going to encounter with an intrinsic error of the library
#The max nº of iterations is bounded and the accuracies will be repeated
for (i in 1:nrow(grid4)){
  svm = svm(Survived~., train, type = "C-classification",
            kernel = "polynomial", degree = grid4$degree[i])
  pred = predict(svm, test, type = "class")
  grid4$accuracy[i] = sum(pred == test$Survived)/length(pred)
}
table(grid4$accuracy[i])
max(grid4$accuracy)

#linear kernel

svm = svm(Survived~., train, type = "C-classification",
            kernel = "linear")
pred = predict(svm, test, type = "class")
accuracy2 = sum(pred == test$Survived)/length(pred)

#At last we will try a radial kernel
grid5 = expand.grid(gamma = 2^(-70:90))
for (i in 1:nrow(grid5)){
  svm = svm(Survived~., train, type = "C-classification",
            kernel = "radial", gamma = grid5$gamma[i])
  pred = predict(svm, test, type = "class")
  grid5$accuracy[i] = sum(pred == test$Survived)/length(pred)
}
grid5[which.max(grid5$accuracy),]

#ggplot(grid5) + aes(x = gamma, y = accuracy) + geom_point(size = 0.3) +
#  geom_line(size = 0.3) + theme(text = element_text(size = 6))
