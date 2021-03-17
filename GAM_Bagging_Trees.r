############## ML2 MidTerm Exam SP20  ###############
#############  Version A Solution     ###############
############### [Student Name Here]  ################

rm(list=ls()) # Clear the global environment
####################################################
### Functions
####################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
###############################
### Load reequired packages ###
###############################
needed <- c('randomForest', 'gam', 'gbm', 'akima')  
installIfAbsentAndLoad(needed)
##########################
####    Setup         ####        
##########################
set.seed(5082)
getwd()
# Create dataframe for tree-based methods
attritionDF <- read.csv("IBMAttrition.csv", stringsAsFactors=T, header=T)
train.indices.attrition <- sample(nrow(attritionDF), nrow(attritionDF) * .8)
attrition.train <- attritionDF[train.indices.attrition, ]
attrition.test <- attritionDF[-train.indices.attrition, ]

# Create a dataframe for gam model
stats <- read.csv("Stats.csv", header=T, row.names='name')
train.indices.stats <- sample(nrow(stats), nrow(stats) * .8)
stats.train <- stats[train.indices.stats, ]
stats.test <- stats[-train.indices.stats, ]

###################################################
set.seed(5082)
# (1) Construct a random forest of 500 trees with mtry set to 4
# Using the training set
# Use attrition as the Y target variable
# Request an importance report and insure that bagging is
# with replacement.

RF <- randomForest(attrition ~ .,
                   data=attrition.train,
                   ntree=500,
                   mtry=4,
                   importance=TRUE, 
                   replace=T)

# (2) Use the random forest model to make predictions on the test set

RF.yhat = predict(RF, 
                  newdata=attrition.test)

# (3) Plot the predicted values against the test set

plot(RF.yhat, 
     attrition.test$attrition)

# (4) Produce the confusion matrix and error rate of the random forest results

#training results
RF

RF$confusion

RF$err.rate

#test results
table(RF.yhat, 
      attrition.test$attrition)

(48+1)/(236+48+1+9)

# (5) What is the most important X feature, according to the random forest model?
# Display the Variable Importance report 

# Either method works
RF$importance #method 1

importance(RF) #method 2

# Plot Variable Importance

varImpPlot(RF)

# Provide your answer in a comment

#MonthlyIncome is the most important for decreasing Gini
#Overtime is the most important for decreasing accuracy

# (6) Construct a boosting model of 500 trees with 4 splits
# Using the training set
# Use attrition as the Y target variable
# Request an importance report 

#First we need to create a numeric version of the attrition variable
#training set
#as.numeric(attrition.train$attrition) produces 1 for No and 2 for Yes
#so we need to shift the distribution by -1



str(attritionDF)

NumAttrition <- as.numeric(attrition.train$attrition) -1
attrition.train <- data.frame(attrition.train, NumAttrition)

#test set
NumAttrition <- as.numeric(attrition.test$attrition) -1
attrition.test <- data.frame(attrition.test, NumAttrition)

#Remove the categorical attrition variable from the X feature list using -attrition
#Use NumAttrition as the Y variable
Boosting    <-   gbm( NumAttrition ~ .-attrition,
                      data=attrition.train,
                      distribution = "bernoulli" ,
                      n.trees=500,
                      interaction.depth=4 ) # 4 splits

# (7) Use the boosting model to make predictions on the test set

Boosting.yhat <- predict(Boosting, 
                        newdata=attrition.test, 
                        n.trees=500, 
                        type="response") # Boosting.yhat are prob values

Boosting.YesNo = ifelse ( Boosting.yhat > .5, "Yes" , "No" )

# (8) Plot the predicted values against the test set

plot(Boosting.yhat, attrition.test$attrition)

# No need to plot YesNo against YesNo
# plot(Boosting.YesNo ~ attrition.test$attrition)

# (9) Produce the confusion matrix and error rate of the boosting results

table(Boosting.YesNo, attrition.test$attrition)

(9+35)/(228+35+9+22) 

# (10) What is the most important X feature, according to the boosting model?
# Display the Variable Importance report 
# Plot Variable Importance

summary(Boosting)

# Provide your answer in a comment
#monthlyincome (not jobrole) is the most important variable

# (11) In a comment, briefly discuss the differences between random forest and boosting results,
# and whether they make sense, given what you know about the two algorithms

#RF produced an error rate of 16.67%
#Boosting produced an error rate of 14.97%
#So overall boosting was superior to RF
#Note that boosting produced more false positives than RF did
#This makes sense because boosting aggregates weak learners
#with a slow learning rate which is supposed to produce better results
#compared to RF

#
# (1) Create a gam to predict salary as a function of crbi,
# catbat and chmrun. 
# 
# Model crbi using a smoothing spline with 5 degrees of freedom 
# Model catbat using a smoothing spline with 6 degrees of freedom 
# Model chmrun using local regression with a 25% span 

MyGam <- gam(salary ~ s(crbi, df=5) + 
             s(catbat, df=6) + 
             lo(chmrun, span=.25),
             data=stats.train)

# (2) On a single plot window, plot the three individual
# relationships. Include standard error bars in the plots.

par(mfrow=c(1, 3))
plot(MyGam, se=T, col="blue", main='GAM #1')
par(mfrow=c(1, 1))

# (3) Using the predict() function, make predictions on the test set.

gam.yhat <- predict(MyGam, newdata=stats.test)

# (4) Compute and display the test MSE

(gam.mse <- mean((gam.yhat-stats.test$salary)^2))

# (5) Extend this gam by adding an interaction term between the
# chits and cwalks variables modeled as a local regression
# with 50% span 

MyGam2 <- gam(salary ~ s(crbi, df=5) + 
              s(catbat, df=6) + 
              lo(chmrun, span=.25) +
              lo(chits, cwalks, span=0.5) ,
              data=stats.train)

# (6) Create individual plots of the four individual
# relationships. Include standard error bars in the plots.

par(mfrow=c(1, 4))
plot(MyGam2, se=T, col="blue", main='GAM #2')

# (7) Using the predict() function, make predictions on the test
# set with this extended model.

MyGam2.yhat <- predict(MyGam2, 
                       newdata=stats.test)

# (8) Compute and display the test MSE of this extended model

( MyGam2.MSE <- mean((MyGam2.yhat - stats.test$salary )^2) )

# (9) Use anova (F-test) to determine if there is statistical 
# evidence that the addition of the interaction term between
# chits and cwalks was a good idea.

anova(MyGam, MyGam2, test="F")

# In a comment, discuss the anova findings and your conclusion

#The extended model is a significant improvement from the base model.
#The addition of the interaction term between chits and cwalks was a good idea
