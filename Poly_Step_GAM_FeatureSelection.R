#Chapter 7 
library(ISLR)
data(Wage)
attach(Wage)
library(tidyverse)
library(splines)

#example of polynomial
fit = lm(wage~poly(age,4))
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])

# set grid of values for age for which we want predictions, in this case, predictions for polynomial for all of age
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
# we select pred$fit to actually reach the elements of interest
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mfrow=c(1,1),mar=c(4.5, 4.5, 1, 1),oma=c(2,2,2,2))
plot(age,wage,xlim=agelims,cex=.5, col="darkgrey")
title ("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
# se.bands are the confidence interval created
# to compare polynomials, use anova() function to perform analysis of variance
fit.1= lm(wage~age,data=Wage)
fit.2= lm(wage~poly(age,2),data=Wage)
fit.3= lm(wage~poly(age,3),data=Wage)
fit.4= lm(wage~poly(age,4),data=Wage)
fit.5= lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

#isolate coefficients like this
coef(summary(fit.5))

#Step Functions
#Ways to choose number of cuts using cross validation on train set with different number cuts

#Here we divide_data using sample, partitioning the data into 70% of the indices randomly
divide_data <- sample(nrow(Wage), nrow(Wage)*0.7)
#subscript divide_data to pull direct Wage data values
train <-Wage[divide_data,]
#test leaves everything else out
test <- Wage[-divide_data,]

#We now calculate the cv_errors manually, to find the optimal amount of steps 
cv_errors <- c()

for (i in 2:10) {
  #we fit 8 different steps in the loop on the training
  steps <- lm(wage~cut(age,i),data=train)
  #use the training data to predict the test information steps
  steps.pred <-predict(steps,newdata=test)
  #compute the residuals from the test and predictions
  res <- test$wage-steps.pred
  #calculate the RMSE manually and fill the RMSE into the cv_errors vector
  cv_errors[i] <- sqrt(mean(res^2))
}


#output the best_cut, or minimal cv_error with associated step index
(best_cut = which.min(cv_errors))

#Plot cv_errors along with steps
plot(cv_errors,type='l',main='Cv_errors per step')

#Calculate fit for best cut
step.fit = lm(wage~cut(age, best_cut), data=Wage)
# summary of step coefficients
coef(summary(step.fit))

# plot method 1
agelims = range(Wage$age)
# sequence over all age lims to compute grid
age.grid = seq(from=agelims[1], to=agelims[2])
# formulate predictions based off best cut, and age.grid
lm.pred = predict(step.fit, newdata = list(age=age.grid))
# plot the predictions of wage based off age
plot(wage~age, data=Wage, col="darkgrey",main='Wage predicted by Age')
lines(age.grid,lm.pred,col='red',lwd=2)

#Second way of plotting using ggplot
test %>% 
  mutate(predictions = predict(step.fit,test)) %>% 
  ggplot(aes(age,wage,col='blue')) + geom_point() +
  geom_line(aes(age,predictions,col='red'),size=1) +
  #need to include values to render labels
  scale_color_manual(name = 'Data Types',labels=c('Observed','Predicted'),values=c('blue','red'))+
  labs(x='Age',y='Wage')

#splines 
#we can define the number of knots
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

#can also use df argument
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

#smoothing splines
smooth_fit=smooth.spline(age,wage,cv=TRUE)
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#local regression
#larger the span, smoother the fit
local_fit=loess(wage~age,span=.2,data=Wage)
lines(age.grid,predict(local_fit,data.frame(age=age.grid)),col="red",lwd=2)

#Applied Exercise 10
rm(list=ls())
library(ISLR)
attach(College)
library(caret)
library(leaps)
library(gam)

data <- createDataPartition(Outstate , p=.8, list = FALSE)  
College.train <- College[data,]
College.test <- College[-data, ]

#Best subset of data
#nvmax set to x features
varfit<-regsubsets(Outstate~.,data=College.train ,method ="forward", nvmax=17)
var.summary<-summary(varfit)
names(var.summary)
# we maximize adjusted r2, or minimize the cp and BIC
# inspect elements
which.min(var.summary$cp)
# we see how many variables are included for each 
par(mfrow=c(1,3))
plot(var.summary$cp, xlab="Variable Count", ylab = "Cp", type= "l")
plot(var.summary$bic, xlab="Variable Count", ylab = "BIC", type= "l")
plot(var.summary$adjr2, xlab="Variable Count", ylab = "Adj. R2", type= "l")

#isolate names of actual variables
# we pick how many variables, and the algorithm will choose the best combination for us --> computationally intensive with large data sets
final <-coef(varfit, id=5)
names(final)

#large for loop to test for different gam outputs with different degrees of freedom
r2_all=c()
for (i in 1:20) {
  gm.1<-gam(Outstate~Private+s(Room.Board, df=i)+s(PhD, df=i)+s(perc.alumni, df=i)+s(Expend,df=i)+s(Grad.Rate, df=i),data=College.train)
  preds<-predict(gm.1,newdata=College.test)
  gamMSE<-mean((College.test$Outstate-preds)^2)
  gamTSS<-mean((College.test$Outstate-mean(College.test$Outstate))^2)
  testR2<-1-gamMSE/gamTSS
  r2_all[i] <- testR2
}
which.max(r2_all) #7

r2_all

gm.1<-gam(Outstate~Private+s(Room.Board, df=10)+s(PhD, df=10)+s(perc.alumni, df=10)+s(Expend,df=10)+s(Grad.Rate, df=10),data=College.train)

par(mfrow=c(2,3))
plot(gm.1, se=TRUE, col='blue')

#evaluation
preds<-predict(gm.1,newdata=College.test)
gamMSE <- mean((College.test$Outstate-preds)^2)
gamMSE

# we also should evaluate gams with the anova() function
anova(gm.1)
#this analyzes linear or parametric effects
# we should also use anova to compare different gam models! 
