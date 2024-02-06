#Data preprocesessing
yash = read.csv("D:\\SJC\\1st SEM\\SASI\\Projects\\Copy of spotify_data_v1(1).csv")
library(tibble)
yash=as.tibble(yash)
class(yash$Age)
class(yash$Revenue)
yash$Age = as.integer(yash$Age)
yash$Revenue= as.numeric(yash$Revenue)
class(yash$Age)
class(yash$Revenue)
#subseting the data by age(18-30)
library(dplyr)
yash1 = subset(yash,Age>=18 & Age<=30)
view(yash1)

min(yash1$Age)
max(yash1$Age)

#age group
yash=yash%>%
  mutate(Age_Group2 = case_when(Age>=0& Age<=18 ~"0-18",
                                Age>=19 & Age<=30~"19-30",
                                Age>=31 & Age<=50~"31-50",
                                Age>=51 & Age<=80~"51-80"))
view(yash)

#visualization of nuemeric variables
library(ggplot2)
yash$Age=as.factor(yash$Age)
yash$Revenue=as.integer(yash$Revenue)
?hist
hist <- ggplot(data = yash1, aes(x = Age, fill = Gender))
hist + geom_histogram(binwidth = 5, color = "white") + 
  labs(y = "Revenue",
       x = "Age",
       title = "VISUALIZATION") + 
  theme_light()

class(yash$Age)
class(yash$Revenue)



ggplot(yash1, aes(x = Gender, y = Revenue, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Box Plot: Age vs Revenue by Gender",
       x = "Gender",
       y = "Revenue") +
  theme_minimal()


ggplot(yash1, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, alpha = 0.7) +
  labs(title = "Age Distribution by Gender",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

yash1 = subset(yash,Gender == "Male" | Gender =="Female")
view(yash1)

my.box <- ggplot(yash1, aes(x = Age_Group, y = Revenue))
my.box + geom_boxplot(outlier.color = "red", outlier.shape = 0.6) + 
  geom_jitter(width = 0.2, aes(color = Gender)) +
  labs(title = "age by revenue") + 
  theme_light()
library(pacman)
names(yash1)


ggplot(yash1,aes(x=Age_Group,color = Age_Group))+
  geom_bar(aes(color = Age_Group))
