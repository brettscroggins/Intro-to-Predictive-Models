# To finish: Problem 1 Write-ups(2), Problem 2 Write-ups(4), Problem 3

#######################################################
# Chapter 2 - Question 10
#######################################################

library(MASS)
attach(Boston)

summary(Boston)
# A - size of data set
dim(Boston)
#result = 506 x 14


# B - pairwise scatter plots
pairs(Boston)


# C - per capita crime rate associations
par(mfrow=c(2,3))
plot(age,crim)
plot(dis,crim)
plot(rad,crim)
plot(tax,crim)
plot(ptratio,crim)


# D - any suburbs with high crime rates? tax rates? puple-teacher ratio?
# comment on range of each precictor.
par(mfrow=c(3,1))
hist(crim,breaks = 10,col='yellow')
crimhi = Boston[crim >=20,]
nrow(crimhi)

hist(tax, breaks=10,col='green')
taxmid1 = Boston[tax>=200,]
taxmid2 = Boston[tax>=450,]
nrow(taxmid1) - nrow(taxmid2)

taxhi = Boston[tax>=650,]
nrow(taxhi)

hist(ptratio,breaks=10,col='blue')
majlo = Boston[ptratio > 14,]
num20hi = Boston[ptratio > 20.51,]
num20lo = Boston[ptratio > 19.51,]
nrow(num20lo) - nrow(num20hi)
nrow(majlo) - nrow(num20hi)


# E - how many of suburbs bound Charles river?
sum(Boston[,'chas'])
# 35 are on the river


# F - median of pupil-teacher ratio of whole data set
# G - lowest median value of owner-occupied homes
summary(Boston)
# Median ptratio = 19.05
# Lowest medv = $5000 (5 but by \$1000's)

lmedv = Boston[medv==5,]
nrow(lmedv)
summary(lmedv)

# H - 7 or more, and 8 or more room dwellings

seven = Boston[rm >= 7,]
nrow(seven)
# 64 with 7 or more

eight = Boston[rm>=8,]
nrow(eight)
# 13 with 8 or more rooms




#######################################################
# Chapter 3 - Question 15
#######################################################


#A - create regression between each variable

summary(Boston)
attach(Boston)

lm.zn = lm(crim~zn)
summary(lm.zn)
#yes

lm.indus = lm(crim~indus)
summary(lm.indus)
#yes

lm.chas = lm(crim~chas)
summary(lm.chas)
#no

lm.nox = lm(crim~nox)
summary(lm.nox)
#yes

lm.rm = lm(crim~rm)
summary(lm.rm)
#yes

lm.age = lm(crim~age)
summary(lm.age)
#yes

lm.dis = lm(crim~dis)
summary(lm.dis)
#yes

lm.rad = lm(crim~rad)
summary(lm.rad)
#yes

lm.tax = lm(crim~tax)
summary(lm.tax)
#yes

lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio)
#yes

lm.black = lm(crim~black)
summary(lm.black)
#yes

lm.lstat = lm(crim~lstat)
summary(lm.lstat)
#yes

lm.medv = lm(crim~medv)
summary(lm.medv)
#yes


# B - create multiple regression model between crim and all others

lm.all = lm(crim~.,data = Boston)
summary(lm.all)
# Yes = zn, dis, rad, black, medv
# No = indus, chas, nox, rm, age, tax, ptratio, lstat
# y = 17.033 + 0.045(zn) - 0.987(dis) + 0.588(rad) - 0.008(black) - 0.199(medv)


# C - create a plot comparing coefficients from linear to multiple

x = c(coefficients(lm.zn)[2],coefficients(lm.indus)[2],coefficients(lm.chas)[2],coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],coefficients(lm.age)[2],coefficients(lm.dis)[2],coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],coefficients(lm.ptratio)[2],coefficients(lm.black)[2], coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])

y = coefficients(lm.all)[2:14]

par(mfrow=c(1,1))
plot (x,y)


# D - check all functions with polynomial regression (max x^3)

lm.zn3 = lm(crim~poly(zn,3))
summary(lm.zn3)
# 1 and 2

lm.indus3 = lm(crim~poly(indus,3))
summary(lm.indus3)
# 1, 2, and 3

# Chase river will not be a valid predictor with polynomials

lm.nox3 = lm(crim~poly(nox,3))
summary(lm.nox3)
# 1, 2, and 3

lm.rm3 = lm(crim~poly(rm,3))
summary(lm.rm3)
# 1 and 2

lm.age3 = lm(crim~poly(age,3))
summary(lm.age3)
# 1, 2, and 3

lm.dis3 = lm(crim~poly(dis,3))
summary(lm.dis3)
# 1, 2, and 3

lm.rad3 = lm(crim~poly(rad,3))
summary(lm.rad3)
# 1 and 2

lm.tax3 = lm(crim~poly(tax,3))
summary(lm.tax3)
# 1 and 2

lm.ptratio3 = lm(crim~poly(ptratio,3))
summary(lm.ptratio3)
# 1, 2, and 3

lm.black3 = lm(crim~poly(black,3))
summary(lm.black3)
# 1

lm.lstat3 = lm(crim~poly(lstat,3))
summary(lm.lstat3)
# 1 and 2

lm.medv3 = lm(crim~poly(medv,3))
summary(lm.medv3)
# 1, 2, and 3




#######################################################
# Chapter 6 - Question 9
#######################################################

library(ISLR)
install.packages('glmnet')
library(glmnet)
install.packages('pls')
library(pls)


# Part A - seperate to test and train data set

set.seed(1)
train = sample(1:dim(College)[1], dim(College)[1]*.8)
test = -train
train = College[train, ]
test = College[test, ]


# Part B - least squares regression for train set

least.fit = lm(Apps~., data = train)
least.pred = predict(least.fit, test)
mean((least.pred - test[,'Apps'])^2)
# MSE = 1075064
sqrt(1075064)


# Part C - Ridge regression with lambda chosen with CV

train.mat = model.matrix(Apps~., data=train)
test.mat = model.matrix(Apps~., data=test)
grid = 10 ^ seq(10, -2, length=100)
ridge = cv.glmnet(train.mat, train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
bestlam = ridge$lambda.1se
bestlam
# Best lambda = 174.75

ridge.pred = predict(ridge, newx=test.mat, s=bestlam)
mean((test[, "Apps"] - ridge.pred)^2)
# MSE = 1115735
sqrt(1115735)


# Part D - Lasso method with lambda chosen with cv

lasso = cv.glmnet(train.mat, train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
bestlam = lasso$lambda.min
bestlam
# Best lambda = 18.74

lasso.pred = predict(lasso, newx=test.mat, s=bestlam)
mean((test[, "Apps"] - lasso.pred)^2)
# MSE = 1112058
sqrt(1112058)

lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(lasso, s=bestlam, type="coefficients")
# Books and F.Undergrad are the only two with zero coeffiencents


# Part E - PCR model with M chosen with CV

pcr.fit = pcr(Apps~., data=train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
# 7 looks to be the approximate ideal number of components

pcr.pred = predict(pcr.fit, test, ncomp=7)
mean((test[, "Apps"] - data.frame(pcr.pred))^2)
# MSE = 1541736
sqrt(1541736)


# F - PLS Model with M chosen by CV

pls.fit = plsr(Apps~., data=train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")

pls.pred = predict(pls.fit, test, ncomp=10)
mean((test[, "Apps"] - data.frame(pls.pred))^2)
# MSE = 1075376
sqrt(1075376)

# G - Comment on results obtained. How accurately can we predict number 
# of college apps? Is there much difference amongst tests?

summary(College$Apps)

#######################################################
# Chapter 6 - Question 11
#######################################################

library(MASS)
library(leaps)
library(glmnet)
library(pls)

# A - Do subset selection, lasso, ridge, and PCR

# Subset selection
predict.regsubsets = function(object, newdata,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best = regsubsets(crim~., data = Boston, nvmax = 13)
coef(regfit.best,10)
k = 10
set.seed(1)
folds = sample(1:k, nrow(Boston),replace = T)
cv.errors = matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
for(j in 1:k){
  best.fit = regsubsets(crim~., data = Boston[folds != j,],nvmax = 13)
  for(i in 1:13){
    pred = predict(best.fit,Boston[folds == j,], id=i)
    cv.errors[j,i] = mean((Boston$crim[folds ==j] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors,2,mean)
rmse = sqrt(mean.cv.errors)
par(mfrow=c(1,1))
plot(rmse, pch = 19, type = "b")
min.ind = which.min(rmse)
min.ind
# 12 is minimum index
rmse[min.ind]
# RMSE = 6.57

# Lasso
x = model.matrix(crim~., data = Boston)
y = Boston$crim
lasso = cv.glmnet(x,y,type.measure = 'mse')
plot(lasso)
lasso$lambda.min
# Best lambda = 0.047
lasso.mse = lasso$cvm[lasso$lambda == lasso$lambda.min]
sqrt(lasso.mse)
# RMSE = 6.56

# Ridge
ridge = cv.glmnet(x,y, type.measure = 'mse',alpha = 0)
plot(ridge)
ridge$lambda.min
# Best Lambda = 0.59
ridge.mse = ridge$cvm[ridge$lambda == ridge$lambda.min]
sqrt(ridge.mse)
# RMSE = 6.55

# PCR
pcr.fit = pcr(crim~., data=Boston, scale=T, validation = 'CV')
summary (pcr.fit)
# 13 Variable RMSE = 6.52

# B - Which model to use? Include validation error, MSE, etc.

# C - Does the chosen model involve all features? Why or why not?




#######################################################
# Chapter 8 - Question 8
#######################################################

library (ISLR)
library (tree)
library(randomForest)

summary(Carseats)

# A - separate into test and sample set.

set.seed(1)
train = sample(1:nrow(Carseats), nrow(Carseats)*0.8)
carseats.test = Carseats[-train,]


# B - Regression tree

tree.carseats = tree(Sales~., data = Carseats[train,])
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats)

yhat.carseats = predict(tree.carseats, carseats.test)
mean((yhat.carseats - carseats.test$Sales)^2)
# MSE = 4.817


# C - Cross-validation to determine optimal tree complexity

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

# Looks like approximately 8 - 10 is the best choice
pruned.carseats8 = prune.tree(tree.carseats, best = 8)
summary(pruned.carseats8)
yhat8 = predict(pruned.carseats8, carseats.test)
mean((yhat8 - carseats.test$Sales)^2)

pruned.carseats9 = prune.tree(tree.carseats, best = 9)
summary(pruned.carseats9)
yhat9 = predict(pruned.carseats9, carseats.test)
mean((yhat9 - carseats.test$Sales)^2)

# 9 produces slightly lower MSE (4.831) than 8 nodes (4.978),
# but both were higher than before

par(mfrow = c(1,1))
plot(pruned.carseats9)
text(pruned.carseats9)


# D - Bagging

bag.carseats = randomForest(Sales~.,
                            data = Carseats[train,],
                            mtry = 10,
                            ntree = 500,
                            importance = T)
yhat.bag = predict(bag.carseats, carseats.test)
mean((yhat.bag - carseats.test$Sales)^2)
# Bagging reduces RME to 2.077

importance(bag.carseats)
# Top three most important in terms of % Increase of MSE are
# Price, ShelveLoc, and Age with Advertising and CompPrice next.


# E - Random Forest

rf.carseats = randomForest(Sales~.,
                           data=Carseats[train,],
                           mtry = 5,
                           ntree = 500,
                           importance = T)
yhat.rf = predict(rf.carseats, carseats.test)
mean((yhat.rf - carseats.test$Sales)^2)
# Random forest produced MSE = 2.175

importance(rf.carseats)
# Top two are clearly Price and ShelveLoc, and next three are
# CompPrice, Advertising, and Age

#######################################################
# Chapter 8 - Question 11
#######################################################

library(gbm)
library(ISLR)

# A - 1000 observation training set
train = c(1:1000)

Caravan$Purchase = ifelse (Caravan$Purchase == 'Yes',1,0)
caravan.train = Caravan[train,]
caravan.test = Caravan[-train,]


# B - Boosting with purchasing. 1000 trees and .01 shrink

set.seed(1)
boost.caravan = gbm(Purchase~., data = caravan.train,
                    distribution = 'bernoulli', n.trees=1000,
                    shrinkage = 0.01)
summary(boost.caravan)
# Three most impactful are Ppersaut, Mkoopkla, and Moplhoog


# C - Boosting to predict data

boost.prob = predict(boost.caravan, caravan.test,
                            n.trees = 1000, type = 'response')
boost.predict = ifelse(boost.prob > .2, 1, 0)
table(caravan.test$Purchase, boost.predict)

# Predicted who end up buying diveded by total predicted
33 / (123 + 33)
# 21.15% predicted to buy actually do.

# Compare to binomial logistic regression predictor
lm.caravan = glm(Purchase~., data = caravan.train, family = 'binomial')
lm.prob = predict(lm.caravan, caravan.test)
lm.predict = ifelse(lm.prob>.2,1,0)
table(caravan.test$Purchase,lm.predict)

# Predicted who end up buying divided by total predicted
15 / (15 + 82)
# 15.46% predicted to buy actually do logistic regression




#######################################################
# Beauty Pays
#######################################################

beauty = read.csv("~/Downloads/BeautyData.csv",header=T)
summary(beauty)


# 1 - Estimate effect of beauty into course ratings

lm.beauty = lm(BeautyScore ~ CourseEvals, data = beauty)
summary(lm.beauty)
plot(beauty$CourseEvals, beauty$BeautyScore)
abline(lm.beauty, col='red')

lm.beauty2 = lm(BeautyScore ~., data = beauty)
summary(lm.beauty2)


# 2 - No way to tell. A new study would be needed.




#######################################################
# Housing Price Structure
#######################################################

houses = read.csv("~/Downloads/MidCity.csv",header=T)


houses$Nbhd = as.factor(houses$Nbhd)
nbhd.model = as.data.frame(model.matrix(Price~., data = houses))
lm.houses = lm(houses$Price ~., data = houses)
summary(lm.houses)

# 1 - Is there a brick house premium? All others  constant.
  # Yes. Brick = $17,323.54

# 2 - Is there a premium for neighborhood 3?
  # Yes. Nbhd3 = $20,534.71

# 3 - Extra premium for brick in neighborhood 3?

lm.nbhd3_brick = lm(houses$Price ~. + Nbhd3 * BrickYes, data = nbhd.model)
summary(lm.nbhd3_brick)
# Yes. Nbhd3 & Brick = $10,192.78


# 4 - Can we combine neighborhood 1 and 2 into a single 'older' neighborhood?

# No. There are too many other contributing factors that go into the price
# of a houses - such as square footage, location, etc. If I were to combine
# these two into one category, that would increase the Bias of the problem.



#######################################################
# What causes what??
#######################################################

