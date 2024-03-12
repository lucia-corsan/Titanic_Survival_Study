setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo 2")
load("titanic_train.Rdata")
data <- titanic.train
View(data)
library("dplyr")

#first of all we will start eliminating duplicates and missvalues
data <- slice(data, -c(which(duplicated(data))))


#As the variable ticket is a number that was randomly created we eliminate 
#it from the data frame

#As the cabin variable is incomplete we will eliminate it from the data
data <- select(data, -Ticket, -Cabin)
########################Fun begins down here, enjoy the ride

#In this code we will evaluate first the hyper tunning with trees 
#Then the cross validation with trees
#finally a mix of both, we´ll also provide graphics

#First of all only hyper tunning
library(rpart)
library(rpart.plot)
library(ggplot2)

n = nrow(data)

set.seed(30)
#We split data into sets train and test

idx_train = sample(1:nrow(data), floor(0.75*nrow(data)))
train = data[idx_train,]
test = data[-idx_train,]
#We create a table with some of the the parameter values
table = expand.grid(minsplit = seq(10,60,10), cp = 2^(0:-10), maxdepth = seq(1, 10, 1))


#In this loop we will tune the best hyper parameters for the tree
for (i in 1:nrow(table)){
  tree1 = rpart(Survived~., train,
               control = rpart.control(minsplit = table$minsplit[i],
                                       cp = table$cp[i]),
                                        maxdepth = table$maxdepth[i])
  pred = predict(tree1, test, type = "class")
  table$accuracy[i] = sum(pred == test$Survived)/length(pred)
}
#We check the best tree value
table[which.max(table$accuracy),]

treebest <- rpart(Survived~.,data,
                  control = rpart.control(minsplit = table$minsplit[which.max(table$accuracy)],
                                          cp = table$cp[which.max(table$accuracy)],
                                          maxdepth = table$maxdepth[which.max(table$accuracy)]))

#rpart.plot(treebest, extra = 2)
prp(treebest, main = "Were the survivors male or female?",
    extra = 106,
    nn = TRUE, # display the nodes
    shadow.col = "#5078d8", # shadows under the leaves
    branch.lty = 3, # draw branches using dotted lines
    branch = .5, # change angle of branch lines
    trace = 1, # print the auto calculated cex, xlim, ylim
    split.cex = 1.2, # make the split text larger than the node text
    split.suffix = "?", # put "?" after split text
    split.box.col = "lightgray", # lightgray split boxes (default is white)
    split.border.col = "darkgray", # darkgray border on split boxes
    split.round = .5)

#Now let´s try K-Cross Validation, with repetitive iterations
rep_accuracy = 0; n_fold = 5; times = 20
for (j in 1:times){
  # reordering
  folds = sample(rep(1:n_fold, length.out = n))
  accuracy = 0
  for (i in 1:n_fold){
    # split
    train = data[folds != i,]; test = data[folds == i,]
    # fit and evaluate
    tree2 = rpart(Survived~., train)
    pred = predict(tree2, test, type = "class")
    acc = (sum(pred == test$Survived)/length(pred))
    # average
    accuracy = accuracy + acc/n_fold
  }
  accuracy2 = rep_accuracy + accuracy/times
}

#Plots
library("ggelegant")
ggplot(table)+aes(x = cp, y = accuracy, color = as.factor(minsplit))+
  geom_point(size = 0.5) + geom_line(size = 0.9) +
  theme_elegante_std()
