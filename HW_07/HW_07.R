#################################################
#  Company    : Stevens 
#  Project    : HW_07 
#  Purpose    : five (5) nodes in the hidden layer, to develop a classification model for the Diagnosis.
#  First Name : Preet
#  Last Name  : Dabhi
#  Id			    : 10459151
#  Date       : 04/16/2022

## Delete all the objects from your R- environment.
#################################################
rm(list = ls())
#load the library
library(neuralnet)

dataFrame<-read.csv("D:/SEM 3/CS 513/HW_07/wisc_bc_ContinuousVar.csv",na.strings = '?')
View(dataFrame)
table(dataFrame$diagnosis)

dataFrame$diagnosis <- factor(dataFrame$diagnosis, levels = c('M','B'),labels = c(1,2))

#To factor the data set
dataFrame<-data.frame(lapply(na.omit(dataFrame),as.numeric))

# To split the data set into test and testing 
idx<-sort(sample(nrow(dataFrame),as.integer(.70*nrow(dataFrame))))
training<-dataFrame[idx,]
test<-dataFrame[-idx,]
#?neuralnet()
model<- neuralnet(diagnosis~.,training[-1], hidden=5, threshold=0.01)

#Plot the neural network
plot(model)

## test should have only the input colum
ann <-compute(model,test)
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)
length(test$diagnosis)
table(ann_cat,test$diagnosis)

wrong<- (test$diagnosis!=ann_cat)
errorRate<-sum(wrong)/length(wrong)
errorRate