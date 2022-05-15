#Module 5 R practice

library(readxl)
library(readr)
library(knitr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(plotrix)
library(rstatix)
library(corrplot)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

#Reading the data into R 
fat <- read.csv("C:\\Users\\kasuk\\OneDrive\\Documents\\fatdata.csv")

str(fat)


#Producing the correlation table with the help of pearson correlation 

#Pearson Correlation 

#To assess the statistical significance 
cor(fat$age, fat$pctfat.brozek, method="pearson")




#Testing the Null hypothesis 
cor.test(fat$age, fat$pctfat.brozek, method="pearson")



#To show the coorelation matrix 
corrplot(cor(fat))



#Showing the distribution of variables 

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

my_data <- fat[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram=TRUE, pch=19)




#plotting the graph between x,y

x<-seq(-10,10, 1)

y<-x*x

plot(x,y)

cor(x,y)



#Producing a regression table with the neck data and brozek variables

#Simple Linear Regression 

fatdata<-fat[,c(1,2,5:11)]

summary(fatdata[,-1])



#Producing a regression table for neck and body fat percentage 

lm1 <- lm(pctfat.brozek ~ neck, data = fatdata)

plot(pctfat.brozek ~ neck, data = fatdata)

abline(lm1)

names(lm1)


#To obtain the values of the regression table 

lm(formula = pctfat.brozek ~ neck, data = fatdata)
summary(lm1)
