setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo titanic")
load("titanic_train.RData")
library(ggplot2)

data = titanic.train
View(data)

levels(data$Survived) <- c("No", "Yes")

au3 <- data$SibSp != 0 & data$Parch != 0
alone <- au3 == FALSE
alone <- factor(alone, labels = c("Accompanied", "Alone"))


install.packages("ggthemes")
library("ggthemes")


q = ggplot(data)+aes(x = Age, color = alone, fill = alone, alpha = 0.5)+
      geom_density(size= 1, linetype = "dashed")+
      scale_color_manual(values=c("#85C7DE", "#B2ABF2"))+
      scale_fill_manual(values=c("#85C7DE", "#B2ABF2"))+
      facet_wrap(alone)+
      theme(legend.position = "none") +
      labs(title = "Do older people tend to go alone more?",
                             subtitle = "",
                             x = "Age", 
                             y = "Density") 
    