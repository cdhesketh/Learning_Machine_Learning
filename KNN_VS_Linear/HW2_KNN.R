library(ISLR)
attach(Wage)
library(MASS)
library(glmnet)
library(assist)
library(pls)
library(gplots)
library(class)

attach(Auto)
auto=Auto
auto=auto[complete.cases(auto), ]

write.csv(auto,"C:\\Users\\User\\Desktop\\School\\Current_Semester\\Math_533\\HW2\\RautoData.csv", row.names = FALSE)

my.knn <- function(k, train, test)
{
  #train should be a two column data set.
  #That trend will follow when we input the test as well.
  n <- nrow(train) #This is number of observations
  if (k >= n || k < 0 || k %% 1 != 0){ #input validation
    print("K must be an integer, positive,
    and smaller than the number of observations!")
  }
  else
  {
    #The code below this line will run for an
    #appropriate value of k.
    predict = rep(0,nrow(test))
    x <- train[,1]
    y <- train[,2]
    for (i in 1:nrow(test)) 
    {
      #We will create a vector that will store the euclidean distance
      #between the observed points and the testing point for
      #each observation that is in the testing data.
      distance <- sqrt((x-test[i,1])^2 + (y-test[i,2])^2)
      #Now we must pick k closest points from the training data.
      #We will extract the value of the response, and
      #find their average
      sort.distance <- sort(distance, decreasing = FALSE, index.return = TRUE)
      sort.distance.index <- sort.distance$ix
      predict[i] <- mean(train[sort.distance.index[1:k],2])
    }
    return(predict)
  }
}

#set seed
set.seed(1)

#random sample for testing set
test_index=sample(1:nrow(auto),78)

#create testing set and training sets
testing=auto[test_index,c(3,4)]
testing=testing[order(testing$displacement),]
learning=auto[-test_index,c(3,4)]

#Isolate only the x terms from testing
plotting=testing[,1]

#running our knn and R's linear model
results=my.knn(20,learning,test=testing)
model=lm(testing$horsepower~testing$displacement)

#use linear model to predict testing data to compare
lnPred = predict(model, newdata = testing)

#combind x and estimates from knn
plotting=cbind(plotting,results)

#plot the points from all and legends
plot(auto[,c(3,4)],col='light grey',pch=16)
points(testing,col=rgb(.3,.8,.9),pch=16)
points(plotting,col='red',pch=16)
abline(model,col='green')
legend(75,230,legend = c("Learning Points","Testing Points","Estimated Point","Linear Model"), col = c("light grey",rgb(.3,.8,.9),"red","green"),pch = c(16,16,16,NA), lty=c(NA,NA,NA,1))

#Show the comparison between KNN and LN and see that KNN did much better.
compare=cbind(sum(abs(testing[,2]-results)),sum(abs(testing[,2]-lnPred)))
colnames(compare)=c("KNN Sum Diff","LM Sum Diff")
compare






































