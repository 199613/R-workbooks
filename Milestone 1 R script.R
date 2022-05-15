# Milestone 1 


# Importing libraries into R
library(readxl)
library(readr)
library(knitr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(tidyr)
library(plotrix)


#Importing the dataset into R 

shop_data <- read.csv("C:\\Users\\shopdata.csv")
View(shop_data)

#cleaning the data 

shopdata_df = data.frame(shop_data)
shopdata_remove = subset(shopdata_df,select = -c(Row_ID,Order_Date,Ship_Date,Postal_Code,
Ship_Mode,State,Customer_ID,Product_ID,Segment))
shopdata_remove


#Removing the columns which do not have values for better analysis 
shopdata_remove1 = na.omit(shopdata_remove,show_col_types = FALSE)
shopdata_remove1



#Descriptive Analysis


shop_quantity=c(mean(shopdata_remove1$Quantity),median(shopdata_remove1$Quantity),
sd(shopdata_remove1$Quantity),min(shopdata_remove1$Quantity),max(shopdata_remove1$Quantity),
(max(shopdata_remove1$Quantity)-min(shopdata_remove1$Quantity)))

shop_discount=c(mean(shopdata_remove1$Discount),median(shopdata_remove1$Discount),
sd(shopdata_remove1$Discount),min(shopdata_remove1$Discount),max(shopdata_remove1$Discount),
(max(shopdata_remove1$Discount)-min(shopdata_remove1$Discount)))


shop_profit=c(mean(shopdata_remove1$Profit),median(shopdata_remove1$Profit),
sd(shopdata_remove1$Profit),min(shopdata_remove1$Profit),max(shopdata_remove1$Profit),
(max(shopdata_remove1$Profit)-min(shopdata_remove1$Profit)))

shop_shipping=c(mean(shopdata_remove1$Shipping_Cost),median(shopdata_remove1$ Shipping_Cost),
sd(shopdata_remove1$ Shipping_Cost),min(shopdata_remove1$ Shipping_Cost),
max(shopdata_remove1$ Shipping_Cost),(max(shopdata_remove1$ Shipping_Cost)-min(shopdata_remove1$ Shipping_Cost)))



Table1 = round(cbind(shop_quantity,shop_discount,shop_profit,shop_shipping),2)

rownames(Table1)=c('Mean','Median', 'SD','Minimum','Maximum','Range')
knitr::kable(Table1,caption = "Descriptive Statistics")


sales = shopdata_remove1
View(sales)

#Visualizations 

#Plotting a Box Plot 


par(mfrow = c(1, 2))
boxplot(sales$Quantity, main = "Order Quantity")
boxplot(sales$Profit, main = "Profit")


#Plotting a box plot for sales & shipping cost

boxplot(sales$Sales, main = "Sales")
boxplot(sales$Shipping_Cost, main = "Shipping Cost")

#plotting a bar plot 

barplot(summary(sales$Order_Priority))


#Plotting a histogram for the order Quantity 

hist(sales$Quantity, xlab = "Quantity", main = "Quantity")



#Plotting a histogram for the profits 

hist(sales$Profit, xlab = "Profit", main = "Profit")



#Barplot for order priority 

barplot(summary(sales$Order_Priority))




# Scatter Plot 

plot(sales$Quantity, sales$Shipping_Cost,
     xlab = "Order quantity", ylab = "Shipping cost")
