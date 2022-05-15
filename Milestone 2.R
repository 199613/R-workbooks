library(readxl)
library(readr)
library(knitr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(plotrix)


#Importing the CSV file data set and naming the dataset as USAdeath
data<- read_csv("C:\\Users\\shopdata.csv")
View(data)

#Created a dataframe for my data set, later used the subset() to remove the coloumns which doesn't have values so that it becomes easy to work with the data.
data_df = data.frame(data)
data_remove = subset(data_df,select = -c(Row_ID,Order_Date,Ship_Date,Postal_Code,Order_ID,Customer_Name,City,
                                        Country,Market,Region,Category,Sub.Category,Produt_Name,Order_priority,Ship_Mode,State,Customer_ID,Product_ID,Segment))
data_remove


summary(data_remove)



#Hypothesis Testing 1
data<- read_csv("C:\\Users\\shopdata1.csv")



#Test 1
t.test(data,mu=1000,alternative="greater")




#Test 2
#Performing hypothesis testing to check whether the mean of the variable quantity of the sales is equal to 5
t.test(data$Quantity,mu=25,alternative = "greater")


#Test 3
#performing two sample t- test to check if the sales from the year 2011-2013 comapred to the start of the year 

t.test(data$Sales,data$Year, mu=5000, alt="two.sided", conf=0.95, var.eq=F, paired=F)




#Test 4
#Considering the sales is greater than 5000 for 
t.test(data$Sales,mu=5000,alternative = "greater")