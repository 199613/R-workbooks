#Module 3 R Practice 

library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyverse)
library(readxl)
library(readr)
library(gridExtra)
library(psych)
library(lattice)
library(readr)

#Reading Data into R


LungCapData_xls_LungCapData_txt <- read_csv("~C:\\Users\\kasuk\\LungCap.csv")

LungCapData=LungCapData_xls_LungCapData_txt
View(LungCapData)

Lungcap=LungCap$LungCap
Height=LungCap$Height
Age=LungCap$Age


#Created a Boxplot 
boxplot(Lungcap, col = "purple", main= "Boxplot of LungCap", col.main= "Black", 
col.lab="Black" , ylab= "Count", border = "Red",  ylim = c(0,20))


#Viewing the class and summary of the attributes in the dataset 
class(Lungcap)
summary(Lungcap)
summary(Height)
summary(Age)



#For Variable LungCap
#One sample t-Test
#H0: mean<8
#oneside 95% confidence interval for mean

TESTLungcap= t.test(Lungcap, mu=8, alternative = "less",conf.level = 0.95)
TESTLungcap
attributes(TESTLungcap)
TESTLungcap$p.value



#null--> mean=8, alternative hypothesis is --> two sided  than confidence level is 95%
TESTLungcap1= t.test(Lungcap, mu=8, alternative = "two.sided",conf.level = 0.95)
TESTLungcap1
attributes(TESTLungcap1)
TESTLungcap1$p.value




# For Variable Age

TESTAge= t.test(Age, mu=12, alternative = "less",conf.level = 0.95)
TESTAge
attributes(TESTAge)
TESTAge$p.value




#null--> mean=13, alternative hypothesis is --> two sided  than confidence level is 95%
TESTAge1= t.test(Age, mu=12, alternative = "two.sided",conf.level = 0.95)
TESTAge1
attributes(TESTAge1)
TESTAge$p.value




#For variable height

TESTHeight= t.test(Height, mu=64, alternative = "less",conf.level = 0.95)
TESTHeight
attributes(TESTHeight)
TESTHeight$p.value



#null--> mean=13, alternative hypothesis is --> two sided  than confidence level is 95%
TESTHeight1= t.test(Height, mu=64, alternative = "two.sided",conf.level = 0.95)
TESTHeight1
attributes(TESTHeight1)
TESTHeight1$p.value





#CI 99%

TESTLungcap3= t.test(Lungcap, mu=8, alternative = "less",conf.level = 0.99)
TESTLungcap3
attributes(TESTLungcap3)
TESTLungcap3$p.value



#null--> mean=8, alternative hypothesis is --> two sided  than confidence level is 95%
TESTLungcap4= t.test(Lungcap, mu=8, alternative = "two.sided",conf.level = 0.99)
TESTLungcap4
attributes(TESTLungcap4)
TESTLungcap4$p.value

TESTAge3= t.test(Age, mu=12, alternative = "less",conf.level = 0.99)
TESTAge3
attributes(TESTAge3)
TESTAge3$p.value

TESTHeight3= t.test(Height, mu=64, alternative = "less",conf.level = 0.99)
TESTHeight3
attributes(TESTHeight3)
TESTHeight3$p.value

TESTHeight4= t.test(Height, mu=64, alternative = "two.sided",conf.level = 0.99)
TESTHeight4
attributes(TESTHeight4)
TESTHeight4$p.value


