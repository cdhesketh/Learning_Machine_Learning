library(ISLR)
library(MASS)
library(glmnet)
library(assist)
library(pls)
library(gplots)
library(class)
library(e1071)
library(caret)
library(rpart)
library(randomForest)
library(gridExtra)

setwd("C:\\Users\\User\\Desktop\\School\\Current_Semester\\Math_533\\Midterm")

#read data in
adult <- read.csv('adult.data', 
                    sep = ',', fill = F, strip.white = T, stringsAsFactors=T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')


head(adult)

rawRow=nrow(adult)
#replace ? with na to remove them
adult[adult=="?"]=NA
adult=na.omit(adult)
noNARow=nrow(adult)

#Find the columns that are too spread out across different catagories
#drop 3,11,12,13,14
for(i in 1:length(adult))
{
  print(paste(i,": ",length(unique(adult[,i]))))
}

#remove the columns that will cause issues
adult=adult[,-c(14)]

#quick check to see if there are any columns that would not be significant based on a 
# generalized linear model
model = glm(income~.,data=adult,family=binomial(link="logit"))
summary(model)

adult[,4:5]

#Can drop education_num
adult=adult[,-c(5)]

#Plot Catagorical Data
plot1=ggplot(adult)+geom_bar(aes(x=workclass))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot2=ggplot(adult)+geom_bar(aes(x=education))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot3=ggplot(adult)+geom_bar(aes(x=marital_status))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot4=ggplot(adult)+geom_bar(aes(x=occupation)) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot5=ggplot(adult)+geom_bar(aes(x=relationship))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot6=ggplot(adult)+geom_bar(aes(x=race))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot7=ggplot(adult)+geom_bar(aes(x=sex))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
grid.arrange(plot1,plot2,plot3,plot4, ncol=2)
grid.arrange(plot5,plot6,plot7, ncol=2)


#Plot Continuous Data
plot8=ggplot(adult)+geom_histogram(aes(x=fnlwgt))
plot9=ggplot(adult)+geom_histogram(aes(x=age))
plot10=ggplot(adult)+geom_histogram(aes(x=capital_gain))
plot11=ggplot(adult)+geom_histogram(aes(x=capital_loss))
plot12=ggplot(adult)+geom_histogram(aes(x=hours_per_week))
grid.arrange(plot8,plot9,plot10, ncol=2)
grid.arrange(plot11,plot12, ncol=2)

#Plot response variable
ggplot(adult)+geom_bar(aes(x=income))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#set up test and train data with 20% in test
testIndexes=sample(1:nrow(adult),size=nrow(adult)*.2)
testData=adult[testIndexes,]
trainData=adult[-testIndexes,]

######################################################################################
#Run linear Logit classification
model = glm(income~.,data=trainData,family=binomial(link="logit"))# "lasso")
sumLog=summary(model)
predLogit=predict.glm(model,newdata=testData,se.fit=T)

trainY=model$y

#Show confusion matrix for Logit
confusion.glmnet(predLogit$fit,newy=testData$income)

######################################################################################
#Decision Tree
dTree=rpart(income~.,data=trainData)
predTree=predict(dTree,testData,type="class")

#Decision Tree Confusion Matrix
confusionMatrix(testData$income,predTree)

######################################################################################
#Random Forests
rForest=randomForest(income~.,data=trainData,ntree=100,imprtance=T)
predForest=predict(rForest,testData)

#Random Forests Confusion Matrix
confusionMatrix(testData$income,predForest)


###################################################################################################
#SVM Model

tuneRND=sample(dim(trainData)[1],1000)
tunout=tune(svm,income~.,data=trainData[tuneRND,],kernel="radial",range=list(cost=1*c(1:9),gamma=10^(-1)*c(1:9)))
tunout

modelSVM=svm(income~.,data=trainData,cost=10,gamma=10^(-1))

#Predict SVM and Confusion Matrix
predSVM=predict(modelSVM,testData)
confusionMatrix(testData$income,predSVM)

#SVM standard cost and gamma
modelSVM=svm(income~.,data=trainData)
predSVM=predict(modelSVM,testData)
confusionMatrix(testData$income,predSVM)





















































