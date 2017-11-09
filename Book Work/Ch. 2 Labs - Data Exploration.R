#Chapter 2 R notes

#sting combination
x = c(1,3,2,5)
x

#define strings x and y with c = combination
x = c(1,6,2)
y = c(1,4,3)

#string operations
length(x)
length(y)
x+y

#define matrix x
x = matrix(c(1,2,3,4),2,2,byrow = TRUE)
x
sqrt(x)
x^2

#Random normal and correlation functions
x = rnorm(50)
x
y = x + rnorm(50, mean=50, sd=.1)
y
cor(x,y)

#set.seed to set the same random variables throughout the experiment
set.seed(1303)
rnorm(50)

#measures of the data
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

#Graphics

#plot function
x = rnorm(50)
y = rnorm(50)
plot(x,y)
plot(x,y,xlab='this is the x-axis', ylab='this is the y-axis', main='Plot of X vs. Y')
pdf('Figure.pdf')
plot(x,y,col='green')
dev.off()

#sequences
x = seq(1,10)
x
x=1:10
x
x = seq(-pi,pi,length=50)
x

#plot and contour
y = x
f = outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

#images
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

#Indexing Data
A = matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]

#loading data
Auto = read.table("http://www-bcf.usc.edu/~gareth/ISL/Auto.data",header = T,na.strings='?')
dim(Auto)
Auto[1:4,]
Auto = na.omit(Auto)

names(Auto)

#These mean the same thing
plot(Auto$cylinders,Auto$mpg)

attach(Auto)
plot(cylinders, mpg)

#barplots
plot(cylinders,mpg,col='red',varwidth=T,xlab='Cylinders',ylab='MPG')
plot(cylinders,mpg,col='red',varwidth=T,horizontal=T,xlab='Cylinders',ylab='MPG')

#histograms
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)

#pairs of variables with the pairs() function
pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration, Auto)

#identify() functions
plot(horsepower,mpg)
identify(horsepower,mpg,name)
