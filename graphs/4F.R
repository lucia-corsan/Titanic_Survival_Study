setwd("C:/Users/Alonso/Desktop/00.ALONSO/Ciencia de datos/Trabajo 1")
load("titanic_train.Rdata")
library(ggplot2)

data = titanic.train
View(data)
levels(data$Survived) <- c("No", "Yes")

au3 <- data$SibSp != 0 & data$Parch != 0

alone <- au3 == FALSE
levels(alone) <- c("No", "Yes")
alone <- factor(alone, labels = c("Accompanied", "Alone"))


lowClass <- c(data$Pclass == 3 & data$Fare <= 14)
medClass <- data$Pclass == 2 & (data$Fare <= 38.5 & data$Fare >= 14)
highClass <- data$Pclass == 1 & data$Fare >= 38.5

# Creating a new column called social class, by repeating our classes in a vector
#and then attaching it to the main dataframe
socClas = rep("Low class",length(lowClass))
socClas[medClass] = "Medium class"
socClas[highClass] = "High class"
titanic.train = cbind(titanic.train, socClas)
data$socClas=factor(data$socClas,
                    levels=c("High class","Medium class","Low class"))

ggplot(data)+aes(x = Embarked, fill = Survived)+
  geom_bar(position = position_fill(), width = 0.9)+
  labs(title = "Where did more people embarked, did that affect survability?",
       subtitle = "",
       x = "Embarking port", #adding titles and labels on x and y
       y = "Proportion")+
  scale_fill_manual(values = c("#d14258","#D4B483"))


ggplot(data)+aes(x = Embarked, fill = alone)+
  geom_bar(position = position_fill())+
  labs(title = "Where did more alone people embarked?",
       subtitle = "",
       x = "Embarking port", #adding titles and labels on x and y
       y = "Proportion")+
  scale_fill_manual(values = c("#d14258","#D4B483"))
