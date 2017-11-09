#Chapter 5 Exercises

#Validation set approach

#set first random sampling
set.seed(1)
train = sample(392,126)

attach(Auto)

#linear regression and its RMS
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Quadratic and its RMS
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic and its RMS
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#do a second random sampling and same process as above
set.seed(2)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

##########################################################
#Leave-One-Out Cross-Validation
##########################################################

#'generalized' linear regression - can fit data into different families
glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)

#linear regression
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)

#finds the cross validation error of glm.fit()
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

#using regression for polynomials of power 1 - 5 and put out cv error for each
cv.error = rep(0,5)
for(i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

##########################################################
#k-Fold Cross-Validation
##########################################################

#MUCH FASTER THAN LOOCV

set.seed(17)
cv.error.10=rep(0,10)

#set 10 k-fold cross validation and check for polynomial 1-10
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}

cv.error.10

##########################################################
#Bootstrap Method
##########################################################

install.packages("ISLR")
library(ISLR)


alpha.fn=function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio,alpha.fn,R=1000)

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset = index)))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace = T))
boot.fn(Auto,sample(392,392,replace = T))

boot(Auto, boot.fn,1000)

summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn = function(data,index)
  coefficients(lm(mpg~horsepower + I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower + I(horsepower^2),data=Auto))$coef
