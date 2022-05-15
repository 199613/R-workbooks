#Libraries
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(plotrix)

#PART 1
#Installing the package MASS and importing it
install.packages("MASS")
library(MASS)

#Checked th dataset cats as mentioned in Part1 of the assignment.
cats
df_cats=cats

#bifercating males and Female cats
male_cats=subset(df_cats,Sex=="M")
female_cats=subset(df_cats,Sex=="F")

#Hypothesis
cat_t.test=t.test(male_cats$Bwt,female_cats$Bwt,alternative="two.sided",var.equal=FALSE)
cat_t.test
#We can see that the p-value is almost 0 and it is 8.831e-15 (which is almost 0), we reject the null hypothesis that male and female cat samples donot have the same bodyweight.

#PART 2
# Importing the data which is given about sleep quality scores before and after workshops.
before_workshop=c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after_workshop=c( 6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

#Hypothesis
t.test(before_workshop,after_workshop,paired = TRUE,conf.level = 0.95)


#for conf.level=0.05
t.test(before_workshop,after_workshop,paired = TRUE,conf.level = 0.05)

#for conf.level=0.1
t.test(before_workshop,after_workshop,paired = TRUE,conf.level = 0.1)


