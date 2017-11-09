#Chapter 3 Labs

install.packages("ISLR")
library(MASS)
library(ISLR)

names(Boston)

#create best fit line for medv and lstat and view its parameters
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit
summary(lm.fit)

#to view lm.fit's names
names(lm.fit)
#to view lm.fit's coefficients
coef(lm.fit)
#to view the confidence intervals of the line
confint(lm.fit)

#predict can be used to create confidence intervals and prediction intervals for the
#prediction of mdev for a given value of lstat
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = "confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = "prediction")

#plot scatterplot along with best fit line
plot(lstat,medv)
abline(lm.fit,col='red',lwd=5)

par(mfrow= c(2,2))
plot(lm.fit)

#computes residuals linear regression from its residuals
plot(predict(lm.fit),residuals(lm.fit))

#rstudent returns - like regression with a leave-one-out spin
plot(predict(lm.fit),rstudent(lm.fit))

#non-linearity evidence, so this function can be used with a number of predictors
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

################################
## Multiple Linear Regression ##
################################

#regression model with lstat and age as predictors of medv
lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)

lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)

#Extra note - summary(lm.fit)$sigma gives us the RSE error
summary(lm.fit)$sigma

install.packages('car')
library(car)
vif(lm.fit)

lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)
lm.fit1 = update(lm.fit,~.,-age)



################################
## Non-linear Transformations ##
################################

#Outputs the summary of the linear regression with Coefficients and Standard Error
summary(lm(medv~lstat*age, data=Boston))



#######################
## Interaction Terms ##
#######################

lm.fit2 = lm(medv~lstat+I(lstat^2))
summary (lm.fit2)

lm.fit = lm(medv~lstat)
#anova function compares the two models under hypothesis test that the null hypothesis is
  #that the two models fit the data equally well.
anova(lm.fit, lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

#for higher order polynomials, use the poly function
  #5th order for example

lm.fit5 = lm(medv~poly(lstat,5))
summary (lm.fit5)



############################
## Qualitative Predictors ##
############################

names(Carseats)

lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

contrasts(Carseats$ShelveLoc)



########################
## Building Functions ##
########################
LoadLibraries = 
function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries()
