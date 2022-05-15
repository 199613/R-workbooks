library(readxl)
library(readr)
library(knitr)
library(dplyr,warn.conflicts=FALSE)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(tidyr)
library(plotrix)
library(RColorBrewer)
library(gmodels)

#Reading data into R, Manually imported dataset
data <- read_excel("C:/Users/kasuk/LungCap.xlsx")
View(data)
class(data)

#Cleaning data, Renaming the column Names for easy understanding
data$Smoking<- data$Smoke

#Removing unnecessary columns
lungcap=subset(data,select=-c(Smoke))
lungcap

#summary of data 
summary(lungcap)


#calculating the descriptive analysis for the dataset
view(lungcap)
Mean1 = c(mean(lungcap$Age))
Mean2 = c(mean(lungcap$Height))
Mean3 = c(mean(lungcap$LungCap))
SD1= c(sd(lungcap$Age))
SD2 = c(sd(lungcap$Height))
SD3 = c(sd(lungcap$LungCap))
Median1 = c(median(lungcap$Age))
Median2 = c(median(lungcap$Height))
Meadian3 = c(median(lungcap$LungCap))
Min1 = c(min(lungcap$Age))
Min2 = c(min(lungcap$Height))
Min3 = c(min(lungcap$LungCap))

#Histogram and Bar plots 
library(ggplot2)
ggplot(lungcap, aes(x = LungCap)) + geom_histogram(color = 'Green')


ggplot(lungcap, aes(x = LungCap, y = Age)) + geom_boxplot()


#Barplot for Age and smoking
ggplot(lungcap, aes(x = Smoking, y = Age)) + geom_boxplot()

#As we have two gender we can create a bar plot and compare the variables 

ggplot(lungcap, aes(x = Gender, y = LungCap)) + geom_boxplot()

#comparing other variables
ggplot(lungcap, aes(x = Gender, fill = factor(Smoking))) + geom_bar(position = position_dodge(1))

ggplot(lungcap, aes(x = Age, fill = factor(Smoking))) + geom_bar(position = position_dodge(1))

#Creating a barplot for gender variable 

ggplot(lungcap, aes(x = Gender)) + geom_bar()


#looking into tables 
ftable_age=ftable (lungcap$Age)
ftable_gender=ftable (lungcap$Gender)
ftable_age
ftable_gender
