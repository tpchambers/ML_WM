# Chapter 8: Boosting
rm(list=ls())
library(tree)
library(MASS)
data(Boston)

#Set up an MSE vector for 3 models
msevector = rep(0,3)

# Row indices for training set (50% split)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Construct the test set
boston.test=Boston[-train,"medv"]

### Section 1: Prune a single tree ###
set.seed(1)

# Fit a regression tree
tree.boston=tree(medv~.,Boston,subset=train)

# Find the best number of notes
cv.boston=cv.tree(tree.boston)

# Look for the smallest deviance
min(cv.boston$dev) # best size = 7

# Prune tree to 5 nodes
pruned.boston=prune.tree(tree.boston,best=7)

# Predict Y hats using pruned tree
yhat = predict(pruned.boston,newdata=Boston[-train,])

# Calculate MSE for pruned tree
msevector[1] = mean((yhat-boston.test)^2) #35.28688

### Section 2: Boosting with GBM ###
set.seed(1)
library(gbm)

# Train the GBM model
boost.boston=gbm(medv~.,
                 data=Boston[train,], 
                 distribution="gaussian",# regression tree, because medv is a numeric variable
                 n.trees=5000, #5000 trees
                 interaction.depth=4) # 4 splits

yhat.gbm= predict(boost.boston,
                  newdata=Boston[-train,],
                  n.trees=5000)

msevector[2]=mean((yhat.gbm-boston.test)^2)


### Section 3: Boosting with xgboost ###
set.seed(1)
library(xgboost)

# xgboost expects data in matrix form, so set up the matrices first
Train.X = data.matrix(Boston[train,-14]) # Exclude medv from the X matrix, which is column 14
Test.X = data.matrix(Boston[-train,-14])
Train.Y = Boston[train,]$medv
Test.Y = Boston[-train,]$medv

# Train an xgboost model
xgboost.boston=xgboost(data=Train.X, # X features have to be in matrix form
                       label= Train.Y, # Y is a vector
                       eta = .5, # learning rate
                       nthread = 1, # number of paralell threads 
                       nround = 5, # number of rounds of predictions
                       object="reg:linear", # regression (for classification, specify "binary:logistic")
                       n.trees=500, # number of trees
                       max.depth=2, # number of splits
                       verbose = 1) # print training error

# Predict y hats using the xgboost model
yhat.boost=predict(xgboost.boston,
                   newdata=Test.X,
                   n.trees=500)

msevector[3]=mean((yhat.boost-Test.Y)^2)
msevector
