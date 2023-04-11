setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo titanic")
load("titanic_train.Rdata")
library(ggplot2)
list.files()

data = titanic.train
View(data)
levels(data$Survived) <- c("No", "Yes")

au3 <- data$SibSp != 0 & data$Parch != 0

alone <- au3 == FALSE
levels(alone) <- c("No", "Yes")
alone <- factor(alone, labels = c("Accompanied", "Alone"))

#people that survived depending on the port?
ggplot(data)+aes(x = Embarked, fill = Survived) +
  geom_bar(position = position_dodge()) + facet_grid(data$socClas) +
  scale_fill_manual(values = c("#d14258","#D4B483")) +
  theme_fivethirtyeight()  

#people that survived depending on the port? (in proportion)
ggplot(data)+aes(x = Embarked, fill = Survived)+
  geom_bar(position = position_fill()) + facet_grid(data$socClas) +
  scale_fill_manual(values = c("#d14258","#D4B483")) +
  theme_fivethirtyeight()  

#people alone depending on the port?
ggplot(data)+aes(x = Embarked, fill = alone)+
  geom_bar(position = position_fill(), alpha = 0.7)+
  labs(title = "Where did more alone people embarked?",
       subtitle = "",
       x = "Embarking port", #adding titles and labels on x and y
       y = "Proportion")+
  scale_fill_manual(values = c("#d14258","#D4B483")) +
  theme_pander()  
