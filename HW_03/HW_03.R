#################################################
#  Company    : Stevens 
#  Project    : HW_03 
#  Purpose    : Perform KNN
#  First Name : Preet
#  Last Name  : Dabhi
#  Id			    : 10459151
#  Date       : 03/1/2022

## Delete all the objects from your R- environment.
#################################################

rm(list=ls())
df <- read.csv("D:/SEM 3/CS 513/HW_02/breast-cancer-wisconsin.csv",header=TRUE, sep=",")
summary(df)

#Here we can see that by running summary on the dataframe F6 column there are some missing values in it, which we omit
n <- as.numeric(as.character(df$F6))
df$F6 <- n

#Remove the rows with missing values 
df <- na.omit(df)

#Replacing class column 2 and 4 with Benign and Malignant
df$Class<- factor(df$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

#KNN

# train and test in the ratio 70% to 30%
size <- sample(1:nrow(df), 0.7 * nrow(df)) 
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
norm <- as.data.frame(lapply(df[,c(2,3,4,5,6,7,8,9,10)], nor))

df2 = df['Class']
#train set
train <- norm[size,] 
cl_train <- df2[size,]
##test set
test <- norm[-size,] 
cl_test <- df2[-size,]

#load the package class
library(class)

#run knn function for k = 3
clf <- knn(train,test,cl=cl_train,k=3)

#create confusion matrix
conf_matrix <- table(clf, cl_test)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

#run knn function for k = 5
clf <- knn(train,test,cl=cl_train,k=5)

#creating matrix
conf_matrix <- table(clf, cl_test)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

#run knn function for k = 10
clf <- knn(train,test,cl=cl_train,k=10)

#create confusion matrix
conf_matrix <- table(clf, cl_test)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)