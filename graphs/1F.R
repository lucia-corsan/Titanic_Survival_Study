setwd("C:/Users/lucia/Documents/Universidad/Introduction/trabajo titanic")
list.files()
load("titanic_train.Rdata")
library("ggplot2")

install.packages("devtools")
devtools::install_github("pmoracho/ggelegant")
library("ggelegant")

data = titanic.train
View(data)
levels(data$Survived) <- c("No", "Yes")

au2 <- data$Sex == "female" | data$Age <= 16 #women_or_child: conditions are
                                             #being a women or underage (according to the
                                             #minority age law from the 1900s)

women_or_child <- au2 == TRUE #if not au2, they are adult men
women_or_child <- factor(women_or_child, labels = c("Men", "Women or children"))


ggplot(data=data,aes(x=Age, fill = Sex, alpha = 0.7)) + 
  geom_histogram(bins = 20, position = position_dodge()) + 
  geom_histogram(bins = 20, position = position_dodge(),aes(y=..count..*(-1))) +
  coord_flip()+facet_wrap(data$Sex)+
  scale_fill_manual(values = c("#1CFEBA","#85e3ff"))+
  scale_y_continuous(labels = abs)+
  theme_elegante_std()

ggplot(data) + aes(x = women_or_child, fill = Survived,)+
  geom_bar(position = position_dodge())+
  scale_fill_manual(values=c("#85e3ff", "#1CFEBA"))+
  theme_elegante_std() + #we add a theme from GitHub
  labs(title = "Does being a woman or a child affects on survability?",
       subtitle = "",
       x = "Did they survive?", #adding titles and labels on x and y
       y = "Number of people")
