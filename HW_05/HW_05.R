#################################################
#  Company    : Stevens 
#  Project    : HW_05 
#  Purpose    : classification and regression tree
#  First Name : Preet
#  Last Name  : Dabhi
#  Id			    : 10459151
#  Date       : 03/17/2022

## Delete all the objects from your R- environment.
#################################################

library(class)
library(rpart)

rm(list=ls())

dataFrame <- read.csv("D:/SEM 3/CS 513/HW_02/breast-cancer-wisconsin.csv",header=TRUE, sep=",")
head(dataFrame, n=5)

#Summarizing each column (e.g. min, max, mean )
summary(dataFrame)

#Here we can see that by running summary on the dataframe F6 column there are some missing values in it, which we omit
n <- as.numeric(as.character(dataFrame$F6))
summary(n, na.rm = TRUE)

dataFrame$F6 <- n
summary(n, na.rm = TRUE)

#Check the number of rows before removing 
nrow(dataFrame)

#Remove the rows with missing values 
dataFrame <- na.omit(dataFrame)
View(dataFrame)
 
#Check the number of rows after removing
nrow(dataFrame)

#Replacing class column 2 and 4 with Benign and Malignant
dataFrame$Class<- factor(dataFrame$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
is.factor(dataFrame$Class)

#Generate train and test in the ratio 70% to 30%
dataFrame<- dataFrame[2:11]
size <- floor(0.70 * nrow(dataFrame))

#Set the seed to make your partition reproducible
set.seed(123)
trainData <- sample(seq_len(nrow(dataFrame)), size = size)

#Loading 70% Breast cancer record in training dataset
training <- dataFrame[trainData, ]

#Loading 30% Breast cancer in test dataset
test <- dataFrame[-trainData, ]

#Implementing CART 
cart <- rpart(Class ~ ., data = training, method = "class")

#Predicting class for test set
predicted <- predict(cart, test, type = "class")
print(length(predicted))
print(length(test$Class))

#Confusion Matrix
conf_matrix <- table(predicted,test$Class)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

