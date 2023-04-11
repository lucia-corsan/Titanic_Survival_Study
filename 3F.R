setwd("C:/Users/Alonso/Desktop/00.ALONSO/Ciencia de datos/Trabajo 1")
load("titanic_train.RData")
library(ggplot2)

data = titanic.train
View(data)
levels(data$Survived) <- c("No", "Yes")

socClas = rep("Low class",length(lowClass))
socClas[medClass] = "Medium class"
socClas[highClass] = "High class"
titanic.train = cbind(titanic.train, socClas) #we combine both, the original data frame and the new condition
data$socClas=factor(data$socClas,
                    levels=c("High class","Medium class","Low class"))

ggplot(data)+aes(x = Fare, y = Age, color = Fare)+
  scale_color_gradient(low = "#EDAFB8", high = "#0C2731")+ #gradient 
  geom_point()+
  labs(title = "How is the relationship between age and fare?",
       subtitle = "",
       x = "Fare", #adding titles and labels on x and y
       y = "Age") +
  theme(axis.text.x = element_text(colour = "#3C7A89", hjust=1), #changing the colour of the axis
        axis.text.y = element_text(colour = "#3C7A89")) +
  geom_smooth(method='lm', col="#2D4E62") #figuring out the expected linear relationship