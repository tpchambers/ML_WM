#Exercise 9
#Decision Trees

#Classification Tree

library(ISLR)
data(OJ)

#split the data 
library(caret)
attach(OJ)

partition <- createDataPartition(Purchase,p=.8,list=F)
train <- OJ[partition,]
test <- OJ[-partition,]

#fit model using tree library
library(tree)

model1 <- tree(Purchase~.,data=train)
summary(model1)

#print split criterion, number of observations in branch, deviance, and prediction for branch
model1

#plot tree model
plot(model1)
text(model1,pretty=0)

#use test data and create confusion matrix
prediction <- predict(model1,test,type='class')
#prediction, then response variable
table(prediction,test$Purchase)
#add diagonals and divide by total
print(paste("Test Error:",((19+26)/length(test$Purchase))))

#Cross Validation to find optimal tree size
# we use predefined tuning parameters calling cv.tree
# for classification, we do prune misclass, otherwise just normal cv which calculates MSE
cvModel <- cv.tree(model1,FUN=prune.misclass)
cvModel
#look for where the size or terminal nodes leads to a lower deviation

#plot
plot(cvModel$size,cvModel$dev,type='b')

#pruned model
# we use prune.misclass for classification, prune.tree for a normal tree
pruneModel <- prune.misclass(model1,best=5)
plot(pruneModel)
text(pruneModel,pretty=0)

#New test error rate
new_predictions <- predict(pruneModel,test,type='class')
table(new_predictions,test$Purchase)
print(paste("Test Error:",((16+27)/length(test$Purchase))))

#Exercise 10 
library(randomForest)
library(tree)
library(MASS)
library(ISLR)
library(gbm)
set.seed(1)

hitters <- ISLR::Hitters

hitters <- hitters[-which(is.na(hitters$Salary)),]
#log to stabilize variance, does not have normal distribution
hitters$Salary <- log(hitters$Salary)

#train and test sets
partition <- sample(1:nrow(hitters),nrow(hitters)/2)

hitters.train <- hitters[partition,]
hitters.test <- hitters[-partition,]

#perform boosting on training set with 1,000 trees and a range for shrinkage parameter lambda
# produce plot with different shrinkage values on x-axis and corresponding training set MSE on y-axis

#for classification, the distribution is bernoulli
boost.model <- gbm(Salary~.,data=hitters.train,distribution='gaussian',n.trees=1000,shrinkage = 0.015)
lambda_values = 10^seq(-9,0,by=.04)
mse.train <- c()
mse.test <- c()

for (i in 1:length(lambda_values)) {
  boost.hitters.train <- gbm(Salary~.,data=hitters.train,distribution='gaussian',n.trees=1000,shrinkage=lambda_values[i])
  pred.boost.test <- predict(boost.hitters.train,newdata=hitters.test,n.trees=1000)
  mse.test[i] <- mean((pred.boost.test-hitters.test$Salary)^2)
}

plot(lambda_values,mse.test,ylab="Test MSE",xlab="Shrinkage Values",type='b',col='forestgreen')
best_mse <- min(mse.test)
best_mse
#calc stdv 
sd = sd(mse.test)
#visualize one standard error rule
abline(h=best_mse+sd,col='red',lty='dashed')
upper_bound = sd+best_mse

#look at variable importance
summary(boost.model)

#apply bagging to training set
#all variables for bagging
#subset must be indices instead of actual dataframe
bag.hitters = randomForest(Salary~.,data=hitters,subset=partition,mtry=19,importance=TRUE)
yhat.bag <-predict(bag.hitters,newdata=hitters.test)
mean((yhat.bag-hitters.test$Salary)^2)

