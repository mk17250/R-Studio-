###### HOMEWORK 317 Q4##################
#a) import mtcars data from R library 
data("mtcars")

#List characteristics of the data frame 
head(mtcars)
mtcars

nrow(mtcars) #number of rows 
ncol(mtcars) #number of columns 
#also:
dim(mtcars) #rows, columns 
length(as.matrix(mtcars)) #numbers of observations 
names(mtcars) #column variable names 
class(mtcars)
attributes(mtcars)
mean(mtcars$mpg) #mean of mpg variable 
sd(mtcars$mpg) #SD of mpg variable 
min(mtcars$mpg) #min value mpg 
max(mtcars$mpg) #max value of mpg 
mean(mtcars$cyl) #mean of cyl variable 
sd(mtcars$cyl) #SD of cyl variable 
min(mtcars$cyl) #min value cyl 
max(mtcars$cyl) #max value of cyl 
mean(mtcars$disp) #mean of disp variable 
sd(mtcars$disp) #SD of disp variable 
min(mtcars$disp) #min value disp 
max(mtcars$disp) #max value of disp 
mean(mtcars$hp) #mean of hp variable 
sd(mtcars$hp) #SD of hp variable 
min(mtcars$hp) #min value hp 
max(mtcars$hp) #max value of hp mean(mtcars$hp) #mean of hp variable 
mean(mtcars$drat) #mean of drat variable
sd(mtcars$drat) #SD of drat variable 
min(mtcars$drat) #min value drat
max(mtcars$drat) #max value drat 
mean(mtcars$wt) #mean of wt variable
sd(mtcars$wt) #SD of wt variable 
min(mtcars$wt) #min value wt
max(mtcars$wt) #max value  wt 
mean(mtcars$qsec) #mean of qsec variable
sd(mtcars$qsec) #SD of qsec variable 
min(mtcars$qsec) #min value qsec
max(mtcars$qsec) #max value  qsec
mean(mtcars$vs) #mean of vs variable
sd(mtcars$vs) #SD of vs variable 
min(mtcars$vs) #min value vs
max(mtcars$vs) #max value vs
mean(mtcars$am) #mean of am variable
sd(mtcars$am) #SD of am variable 
min(mtcars$am) #min value am
max(mtcars$am) #max value am
mean(mtcars$gear) #mean of gear variable
sd(mtcars$gear) #SD of gear variable 
min(mtcars$gear) #min value gear
max(mtcars$gear) #max value gear
mean(mtcars$carb) #mean of carb variable
sd(mtcars$carb) #SD of carb variable 
min(mtcars$carb) #min value carb
max(mtcars$carb) #max value carb

y <- mtcars$mpg #set y variable to mpg

x1 <- mtcars$disp #set x1 variable to displacement 

x2 <- mtcars$hp #set x2 to horse power

x3 <- mtcars$drat #set x3 to rear axle ratio

x4 <- mtcars$wt #set x4 to weight 

###### create scatter plots of each explanatory variable against the response variable to visualize relationship ###
par(mfrow=c(4,1))

plot1 <- plot(x1,y, main ="Scatterplot of mpg vs dispacement")
abline(lm(y~x1), col="blue", lwd=2)

plot2 <- plot(x2,y, main ="Scatterplot of mpg vs horse power")
abline(lm(y~x2), col="blue", lwd=2)

plot3 <- plot(x3,y, main ="Scatterplot of mpg vs rear axle ratio")
abline(lm(y~x3), col="blue", lwd=2)

plot4 <- plot(x4,y, main ="Scatterplot of mpg vs weight")
abline(lm(y~x4), col="blue", lwd=2)

#### I will now create the first linear model containing mpg and horsepower###
lmodel1 <- lm(y~x1+x2)
summary(lmodel1)

#### I will now create the second linear model containing the additional explanatory variables drat and wt ##### 
lmodel2 <- lm(y~x1+x2+x3+x4)
summary(lmodel2)

## Intercept of multivariate model1
B0 <- lmodel1$coefficients[1]
B0

## B1 of model1
B1 <- lmodel1$coefficients[2]
B1

B2 <- lmodel1$coefficients[3]
B2

## Find the fitted values of out Y variable 
model1_fitted_y <- lmodel1$fitted.values
model1_fitted_y

## Find the residuals of out model, that being the distances between the predicted y and observed y;
model1_residuals <-lmodel1$residuals
model1_residuals

model1_residuals_sqrd <- model1_residuals**2
model1_residuals_sqrd

model1_SSR <- sum(model1_residuals_sqrd)
model1_SSR

std_res_model1 <- rstandard(lmodel1)
std_res_model1

plot(model1_fitted_y, std_res_model1, pch=16, ylim=c(-3,3),xlab="fitted y", ylab="standardised residuals")
abline(h=0)
abline(h=-2, lty=2)
abline(h=2, lty=2)

anova(lmodel1, lmodel2)

qf(2, 27, p=0.95)
