#Chapter 6 Exercise 9#
library(ISLR)
data("College")
attach(College)
library(caret)
library(glmnet)
library(pls)

#Least Squares
#training set for least squares 
#partition for training indices
data_partition = createDataPartition(College$Apps,p=.75,list=FALSE)

training <- College[data_partition,]
testing <- College[-data_partition,]

linear_model = lm(Apps~., data=training)
#make predictions, predict command should only contain the testing data and omit data='' call
pred = predict(linear_model,testing)
#MSE manually
mean((testing$Apps-pred)^2)
#summary of train residuals
summary(linear_model)

#Ridge and Lasso
#need model matrix for ridge and lasso
#(b)
x = model.matrix(Apps~.,College)[,-1] #remove first column which contains intercept, exclude apps as well
y = College$Apps

#sample all possible row indices, but only take half of data into train set
train <- sample(1:nrow(x),nrow(x)/2)
#test is everything that is not in train
test = (-train)
y.test = y[test]

#cross validation to choose lambda
cv.out = cv.glmnet(x[train,],y[train],alpha = 0)
plot(cv.out)
#best lambda
(bestlam <- cv.out$lambda.min)
ridge.mod <- glmnet(x[train,],y[train],alpha=0)
ridge.pred <- predict(ridge.mod,s=bestlam,newx = x[test,])
mean((ridge.pred-y.test)^2)

#lasso 
#same steps as before except with alpha = 1 
cv.out <- cv.glmnet(x[train,],y[train],alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x[train,],y[train],alpha = 1)
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

#to inspect coefficients
out = glmnet(x,y,alpha=0)
lasso.coef <- predict(out,type="coefficients",s=bestlam)
lasso.coef


#PCR Model
pcr.fit=pcr(Apps~.,data=College,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")

pcr.fit<-pcr(Apps~.,data=College,subset=x[train,],scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

pcr.pred=predict(pcr.fit,x[test,],ncomp=16)
mean((pcr.pred-y.test)^2)

#pcr 
pls.fit<-plsr(Apps~.,data=College,subset=x[train,],scale=TRUE,validation="CV")
validationplot(pls.fit,val.type='MSEP')
pls.pred = predict(pls.fit, x[test,], ncomp=6)
mean((pls.pred - y.test)^2)

#manually calculate r squared
test.avg = mean(y.test)
ridge.test.r2 = 1-mean((y.test-ridge.pred)^2)/mean((y.test-test.avg)^2)
lasso.test.r2 = 1-mean((y.test-lasso.pred)^2)/mean((y.test-test.avg)^2)
pcr.test.r2 = 1 - mean((y.test-pcr.pred)^2)/mean((y.test-test.avg)^2)
pls.test.r2= 1 - mean((y.test-pcr.pred)^2)/mean((y.test-test.avg)^2)

rbind(c("Ridge", "Lasso", "PCR", "PLS"),
      c(ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2))
