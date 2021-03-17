rm(list=ls())

#Theo Chambers - ML2 Assignment1 # 

##########################
#### Dataframe Setup  ####        
##########################
set.seed(5082)
library(tidyverse)
library(caret)
library(glmnet)
# (1) Create dataframe and model matrix for Ridge & Lasso Models
### No need to write any code or do anything for step (1)    ###
###################### Just skip step (1) ######################


# (2) Read in the data file zillowDF.csv as a dataframe

data <- read.csv("zillowDF.csv",stringsAsFactors = TRUE)


# (3) Make sure qualitative variables are coded as factors
str(data)
print(paste("We see that Type is coded in with two factors."))

# (4) Calculate the age of the house variable ("age") and add the age variable to the dataframe
data %>% mutate(age = 2021 - Year) -> zillow_data
zillow_data
# (5) Create the X matrix using all features except for Price, and the Y matrix using the Price variable, that glmnet expects

x = model.matrix(Price ~.,zillow_data)[,-1]
y = zillow_data$Price


# (6) Create the train (75%) and test (25%) data sets 

#This partitions the data to choose random indices, which we will use as subscripts later to pull actual data from
train <- floor(sample(1:nrow(x),.75 * nrow(x)))
test =(-train)
#train <- zillow_data[divide_data,]
#test <- zillow_data[-divide_data,]


###################################################
####    QUESTION 1: Lasso                      ####        
###################################################
#The objective is to run lasso regression and tune the lamba parameter

set.seed(5082)

# (7)	Create a lamba grid vector of 120 elements ranging from 10^10 to 10^-3 
grid = 10^seq(10,-3,length = 120)


# (8)	Using the glmnet() function, create a lasso model named mod.lasso that predicts the training y's using the training x's and the grid of lambda values created above.

mod.lasso <- glmnet(x[train,],y[train],alpha = 1, lambda = grid)


#Evaluate training model performance using cross-validation
# (9)	Using the cv.glmnet() function and the same parameters used above in the creation of mod.lasso (i.e. including the lambda grid vector),
#     create a 12-fold cross-validation model named cv.out.lasso

cv.out.lasso<- cv.glmnet(x[train,],y[train],alpha = 1,nfolds = 12)


#Visually inspect model performance with a plot
# (10) Plot these model performance values against the log of lambda
plot(cv.out.lasso)

# (11) Display the best cross-validated lambda value (the one that produces the lowest deviance - do not use the 1-standard error rule here).

(bestlam <- cv.out.lasso$lambda.min)


#Make predictions using the best model
# (12) Using the best lambda and the model named mod.lasso, create a vector of test set predictions.

test =(-train)
y.test= y[test]

lasso.pred <- predict(mod.lasso,s=bestlam,newx = x[test,])

# (13) Compute and display the test error rate.

mean((y.test-lasso.pred)^2)
# (14) Display the coefficients of the model associated with the best lambda.
out <- glmnet(x,y,alpha=1)
final_predict <- predict(out,type="coefficients",s=bestlam)[1:ncol(zillow_data),] 
final_predict



# (15) Which variables did Lasso eliminate by shrinking their coefficients to 0? Count variables with dummy variables only if all the coefficients of the variable's levels have been shrunken to 0.
print(paste("The lasso model shrunk the typehouse, year, parking, miles, bath, age, and walkscore all to zero.Type is a factor with 2 levels, so as the coefficient of typehouse was shrunk to zero, the typecondo must be included as it is a dummy variable."))
###################################################
####    QUESTION 2: Ridge                      ####        
###################################################
# (16)	Using the glmnet() function, create a ridge model named mod.ridge that predicts the training y's using the training x's and the grid of lambda values created above.
mod.ridge <- glmnet(x[train,],y[train],alpha = 0, lambda = grid)


#Evaluate training model performance using cross-validation
# (17)	Using the cv.glmnet() function and the same parameters used above in the creation of mod.lasso (i.e. including the lambda grid vector),
#     create a 12-fold cross-validation model named cv.out.ridge

cv.out.ridge<- cv.glmnet(x[train,],y[train],alpha = 0,nfolds = 12)
plot(cv.out.ridge)

#Make predictions using the best model
# (18) Using the best lambda and the model named mod.lasso, create a vector of test set predictions.

(bestlam2 <- cv.out.ridge$lambda.min)

ridge.pred <- predict(mod.ridge,s=bestlam2,newx = x[test,])


# (19) Compute and display the test error rate.

(mean((y.test-ridge.pred)^2))

# (20) Which model selection method performed better, ridge or lasso? Why?

((mean((y.test-ridge.pred)^2)) < (mean((y.test-lasso.pred)^2)))

print(paste("As we can see, the ridge model performed better.This most likely due to the way the lasso model works. Because lasso models actually get rid of coefficients, the MSE may be higher due to omitting variables of small significance. Thus, the ridge produced a closer fitting model when applied to the test set."))

#out<-glmnet(x,y,alpha = 1)
#predict(out,type = "coefficients",s=bestlam)


