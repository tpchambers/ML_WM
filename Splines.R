rm(list=ls())

##################################################################
### Function for installing missing packages                  ####
##################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

##################################################################
####              Load required packages                      ####
##################################################################

# splines and boot are for the spline models
# tree is for the tree models
# Add any additional package you need (e.g., caret) to the needed list below
needed <- c('splines', 'boot','tree') 
installIfAbsentAndLoad(needed)

##################################################################
####              Part A: Regression Spline                   ####        
##################################################################

set.seed(5082) # Do NOT change seed number!

library(splines)
library(boot)

# Create dataframe for Splines
my.spline.data <- read.table('MySplineData.csv', header=T, sep=",")

# Evaluate the cross-validated MSEs of 12 Regression Splines using knots from 1 to 12. 
# The following notes describe the nature of the models to evaluate:
  
# (1) Create an error vector of 12 elements all with zero values

attach(my.spline.data)
error_vector <- c(rep(0,12))

# Using a loop:
# (2a) fit 12 degree-4 regression spline models using the entire dataframe (i.e. no train/test split)
# (2b) compute the cross-validated MSE for each model using 10-fold validation
# Hint: you can do this with just 2 statements inside the loop.
# Note: You may receive warning messages due to some x-values being beyond boundary knots. This is an artifact of the cv process and may be ignored.




for (i in 1:12) {
  fit <- glm(y~bs(x,df=4+i,degree=4),data=my.spline.data)
  error_vector[i] <- cv.glm(data=my.spline.data,fit,K=10)$delta[1]
}




# Evaluate cross validation results
# (3)	Display the 12 cross-validated MSEs
(error_vector)


# (4)	Plot the 12 cross-validated MSEs

plot(error_vector,xlab='knots',ylab='mse',type='l')



# (5) What is the best number of knots and why? Provide your answer in a comment line. 
# This indicates we should use a 2-knot model. 
which.min(error_vector)
print(paste("This indicates we should use 12 knots for the degree 4 spline. We derive the optimal number of knots by computing different MSE values for knot values. In this case, as evident from the graph, the lowest MSE is given by 12 knots added to a 4 degree spline."))


# Make predictions using the best model
# (6)	Fit a final model on the full dataframe with this best number of knots.
final_model <- glm(y~bs(x,df=4+which.min(error_vector),degree=4),data=my.spline.data)


# (7)	Create a vector of 50 new x-values beginning with the minimum value of the sample's x's and ending with the maximum value of the sample's x's.

new_vector <- seq(min(my.spline.data$x),max(my.spline.data$x),length.out=50)

# (8)	Use the final model to predict y-values for these 50 new x's

pred <- predict(final_model,newdata=data.frame(x=new_vector))
##################################################################
####               Part B: Smoothing Spline                   ####        
##################################################################

# (9) Now let's fit smoothing splines with cross-validation

best_spline <- smooth.spline(x,y,cv=TRUE)

# (10) Display number of degrees of freedom in the cross-validated smoothing spline.
best_spline$df

# (11) Use the best model to predict y-values for the 50 new x's from step (7) above
smoothing_pred <- predict(best_spline,new_vector)
smoothing_pred$y

# Visually inspect the plot of predicted values against X values
# (12)	Plot the full sample's x's and y's.

plot(my.spline.data)


# (13)	Add lines that plot the predicted y-values against the 50 new x's. 
# Use red color to indicate the regression spline from Part A
# Use blue color to indicate the smoothing spline from Part B
# Include a legend to identify which is which.

lines(new_vector,pred,col='red')
lines(new_vector,smoothing_pred$y,col='blue')
legend('topright',legend=c('Regression Spline', 'Smoothing Spline'),col=c('red','blue'), lty=1:2, cex=0.8)

##################################################################
####                  Part A: Tree Methods                    ####        
##################################################################

set.seed(5082) # Do not change the seed number!
library(tree)

# Create dataframe for tree-based methods
#read in characters as factors, and ensure that the response variable is coded in as a factor with two levels
attrition <- read.csv("EmployeeAttrition.csv", header=T,stringsAsFactors = TRUE)
attrition$attrition <- as.factor(attrition$attrition)
attrition <- na.omit(attrition)
train.indices <- sample(nrow(attrition), nrow(attrition) * .8)
train <- attrition[train.indices,]
test <- attrition[-train.indices,]

#double check to make sure all characters are read in as factors
str(attrition)


# (14) Fit a classification tree() model predicting attrition classes using the training dataset

tree.model <- tree(attrition ~.,data =train)
plot(tree.model)
#initial classification tree classes
text(tree.model,pretty =0)

# (15) Use the tree() model to make predictions on the test dataset
#      Display the confusion matrix

tree.pred = predict(tree.model,test,type='class')
attrition.test = attrition$attrition[-train.indices]
#confusion matrix
table(tree.pred,attrition.test)


# (16) Next perform cross-validation (cv) to determine the optimal level of tree complexity

cv.treemodel = cv.tree(tree.model,FUN=prune.misclass)
cv.treemodel



# (17) Based on the cv results, what the best number of terminal nodes? Put your answer in a comment line
#We look for the number of terminal nodes that result in the lowest deviation per validation
min(cv.treemodel$dev)
print(paste("Based on the cv results, the best number of terminal nodes are 10. This is because the associated deviation (cross validated error) for 10 terminal nodes is the lowest."))


# (18) Prune the tree based on cv results

prune.treemodel = prune.misclass(tree.model,best=10)



# (19) Use the pruned tree to make predictions on the test dataset
#      Display the confusion matrix

prune.pred = predict(prune.treemodel,test,type='class')
#confusion matrix 
table(prune.pred,attrition.test)

# (20) What is the most important predictor of attrition classes, and its splitting criterion? 
#      (20a) Use a command to show the information you need
#      (20b) And put your answer in a comment line
plot(prune.treemodel)
text(prune.treemodel,pretty =0)
print(paste("According to the pruned tree, the most important predictor of attrition classes is whether or not someone works for overtime, or the feature at the top of the decision tree."))

