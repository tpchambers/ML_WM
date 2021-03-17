library(MASS)
data(Boston)
attach(Boston)
library(leaps)

full_selection = regsubsets(medv~.,nvmax=13,data=Boston)
forward = regsubsets(medv~.,nvmax=13,method='forward',data=Boston)
summary(forward)
#check to see which variables it selects for each amount of variables allowed
backward = regsubsets(medv~.,nvmax = 13, method='backward',data=Boston)

#check coefficients from best 7
coef(backward,7)
coef(forward,7)
coef(full_selection,7)

#can check certain statistics
summary(backward)$rsq
names(summary(backward))

#boolean values to perform cross validation
train=sample(c(TRUE ,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)

#can calculate best regsubsets now with train
regfit1 = regsubsets(medv~.,data=Boston[train,],nvmax=13)
#need model matrix for test 
test_matrix = model.matrix(medv~.,data=Boston[test,])

#basically a long way of calculating MSE on test set for validation
cv_errors <- c()
for (i in 1:13) {
  coef_i = coef(regfit1,id=i)
  #multiply all rows containing pertaining data from test with coefficients made from each ith variable addition
  pred = test_matrix[,names(coef_i)]%*%coef_i
  cv_errors[i] = mean((Boston$medv[test]-pred)^2)
}


which.min(cv_errors)
# looks like 13 variables minimizes the test errors from regsubset


#cross validation for stepwise

k=11
set.seed(1)
folds=sample(1:k,nrow(Boston),replace=TRUE)
cv.errors= matrix(NA,k,13, dimnames = list(NULL, paste(1:13)))

#predict function, R does not have a predict function for regsubsets
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k <- 11
for(j in 1:k){
  best.fit <- regsubsets(medv~.,data=Boston[folds!=j,],nvmax=13)
  for(i in 1:13){
    pred=predict(best.fit ,Boston[folds ==j,],id=i)
    cv.errors[j,i]= mean((Boston$medv[ folds==j]-pred)^2)
  }
}
