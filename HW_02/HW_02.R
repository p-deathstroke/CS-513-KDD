#################################################
#  Company    : Stevens 
#  Project    : HW_02 
#  Purpose    : Perform the EDA analysis
#  First Name : Preet
#  Last Name  : Dabhi
#  Id			    : 10459151
#  Date       : 02/15/2022
#  Comments   : Follow the below steps

## Delete all the objects from your R- environment.
rm(list=ls())
#################################################
##   Step:
## I. Summarizing each column (e.g. min, max, mean )
## II. Identifying missing values
## III. Replacing the missing values with the “mean” of the column.
## IV. Displaying the frequency table of “Class” vs. F6
## V. Displaying the scatter plot of F1 to F6, one pair at a time
## VI. Show histogram box plot for columns F7 to F9
################################################# 

## 1-Load the “breast-cancer-wisconsin.data.csv” from canvas into R 
data <- read.csv("D:/SEM 3/CS 513/HW_02/breast-cancer-wisconsin.csv", na.strings = "?")

## I. Summarizing each column (e.g. min, max, mean )
summary(data)

## II. Identifying missing values
is.na(data)
View(data)

## III. Replacing the missing values with the “mean” of the column
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
View(data)

## IV. Displaying the frequency table of “Class” vs. F6
displayTable <- table(data$Class, data$F6)
ftable(displayTable)

## V. Displaying the scatter plot of F1 to F6, one pair at a time
plot(data[2:7], main = "Scatter Plot of F1 to F6, one pair at a time", ph = 10, col = 2)

## VI. Show histogram box plot for columns F7 to F9
boxplot(data[8:10], main = "Histogram Box Plot for Columns F7 to F9",xlab="X axis", ylab="Y axis")

## 2-Delete all the objects from your R- environment.
rm(list=ls())

## Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
data <- read.csv("D:/SEM 3/CS 513/HW_02/breast-cancer-wisconsin.csv", na.strings = "?")

##Remove any row with a missing value in any of the columns.
data[,1:11][data[,1:11]=="?"] <- NA
data <- na.omit(data)