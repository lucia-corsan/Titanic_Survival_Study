#Supervivencia por clase baja y seleccion de los mas ricos 
#y mas pobres


#Setting working directory loading and renaming data
#setwd("C:/Users/Alonso/Desktop/00.ALONSO/Ciencia de datos/Trabajo 1")
setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo titanic")
list.files()
load("titanic_train.Rdata")
#library(ggplot2)
#library(gganimate)
#library(gifski)

data = titanic.train
View(data)
#Cleaning and renaming some variables
levels(data$Survived) <- c("No", "Yes")

#using an auxiliar to eventually compute percentiles and outliers
au0 <- sort(unique(data$Fare))
#Defining our 3 social classes with the previous percentiles (33% & 66%)
lowClass <- c(data$Pclass == 3 & data$Fare <= 14)
medClass <- data$Pclass == 2 & (data$Fare <= 38.5 & data$Fare >= 14)
highClass <- data$Pclass == 1 & data$Fare >= 38.5
# defining our "outliers" (10% & 90%)
topRich <- data$Pclass == 3 & data$Fare <= 7.7333
topPoor <- data$Pclass == 1 & data$Fare >= 108.9

# Creating a new column called social class, by repeating our classes in a vector
#and then attaching it to the main dataframe
socClas = rep("Low class",length(lowClass))
socClas[medClass] = "Medium class"
socClas[highClass] = "High class"
titanic.train = cbind(titanic.train, socClas)
titanic.train$socClas=factor(titanic.train$socClas,
                    levels=c("High class","Medium class","Low class"))

# Creating a new column of outliers, by repeating our classes in a vector
#and then attaching it to the main dataframe

Topbottom = rep("Normal", length(topPoor))
Topbottom[topRich] = "Top rich"
Topbottom[topPoor] = "Top bottom"
titanic.train = cbind(titanic.train, Topbottom)

#plotting
library("ggplot2")
p = ggplot(data)+aes(x = socClas, fill = Survived)+
  geom_bar(position = position_dodge(), width = 0.9)+ #changing bar position
  labs(title = "How does social class impact in survability?",
       subtitle = "",
       x = "Social class", #adding titles and labels on x and y
       y = "Count")+
  scale_fill_manual(values = c("#F3F7F0","#4E598C")) +
  theme(axis.text.x = element_text(angle=30, hjust=1),
        legend.title = element_text(colour="black", size=11, face="bold"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.box.margin = margin(116, 6, 6, 6),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"))
       #changing letter tilt

library(gganimate)

p + transition_states(states = socClas, transition_length = 2, state_length = 1) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')


ggplot(data)+aes(x = Topbottom, fill = Survived)+
  geom_bar(position = position_fill())+
  labs(title = "How does extreme social values impact in survability?",
       subtitle = "Classified depending on the socioeconomic outliers",
       x = " ",
       y = "Proportion")+
  scale_fill_manual(values = c("#467599","#D44D5C"))+
  theme(axis.text.x = element_text(angle=60, colour = "#CB8C67", hjust=1),
        axis.text.y = element_text(colour = "#CB8C67"),
        legend.background = element_rect(fill="white", size=.5, linetype="dotted", colour = "#CB8C67"),
        legend.title = element_text(colour="black", size=11, face="bold"),
        panel.background = element_rect(fill = "#F5E9E2",
                                        colour = "#F5E9E2"))
    
#Wondering if the previous results of survability are influenced by sex
#Factor that is crucial when studying survability

ggplot(data)+aes(x = Topbottom, fill = Sex)+
  geom_bar(position = position_fill())+
  labs(title = "What are the sex proportions in the outliers?",
       subtitle = "Classified depending on the socioeconomic outliers",
       x = " ",
       y = "Proportion")+
  scale_fill_manual(values = c("#3DD6D0","#3A4454"))+
  theme(axis.text.x = element_text(angle=60, colour = "#7CAFC4", hjust=1),
        axis.text.y = element_text(colour = "#7CAFC4"),
        legend.background = element_rect(fill="white", size=.5, linetype="twodash", colour = "#7CAFC4"),
        legend.title = element_text(colour="black", size=11, face="bold"))

#Indeed the fact of being more men than women on the top richest persons
#affects to the whole survability.
#As more women were top poor the were more likely to survive as a set