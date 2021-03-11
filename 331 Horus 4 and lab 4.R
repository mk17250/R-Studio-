####### HoRus 4 ######

##### FUNCTIONS ######
# A function is a predefined set of actions/commands that can be called by the functions name
# R has many prebuilt functions built into R which can be used immediately and many more which can be added
#via installing packages (more about this later)

# Examples of functions that we have already encountered are sum(), seq(), matrix(), sqrt(), and head(). 

#eg
mean(x = c(-8, 5, 7,-3,15))
?mean #explains function 

#The round function has two arguments which are passed too it. 
?round
round(8.26354, 3) #last argument 'digits' sets what you round it to. 

######### CREATING OUT OWN FUNCTIONS #########
#We can create our own functions by using the 'function()' code, we must define nout functions: 
## for example: function_name <- function(argument1, argument2, ....){
#}

#here is an example using BMI:
BMI <- function(h, w){
  Index <- w/h**2
  return(Index)
}
#####NOTICE the identation is done for us but is important as are the curly brackets dictating what is in the function:
## we can now call the function
BMI(1.79, 74)

### Create a function which calulates the sum of the first n positive integers, such that n is an input set by the user.
#Then apply the function whne n = 78

sum.n<- function(n){
  N <- n*(n+1)/2
  if (n>=0){
    cat('Total')
  } else {
    stop("Positive integers only!!!")
  }
  return(N)
}
sum.n(3)

?all

############################################ LAB 4 ############################################################################
################ Exercise 1 ####################
# generate a dataframe with the following data:
DT <- data.frame(id = 1:10,
                 ht=c(155, 152, 164, 175, 193, 203, 190, 183, 155, 169),
                 wt=c(80, 85, 45, 69, 86, 110, 106, 96, 90, 89),
                 gender=c("m", "m", "f", "m", "f", "f", "f", "m", "f", "m"))
DT

DT$bmi <- DT$wt/(DT$ht/100)^2
DT$bmi.grp <- ifelse(DT$bmi <= 18.5, "underweight", "obesity")
DT$bmi.grp <- ifelse(DT$bmi > 18.5 & DT$bmi <= 25, "normal", DT$bmi.grp)
DT$bmi.grp <- ifelse(DT$bmi > 25 & DT$bmi <= 30, "overweight", DT$bmi.grp)

e <- 2.32654
round(e, digits = 2)
round(e, 2)


################## EXERCISE 2 ###########################################
## A) load the internal R data set mtcars and view its help page to find out about its varaiable descriptions 

data("mtcars")
str(mtcars)

?mtcars

## B) Fit a linear regression model of "Miles per gallon" on "Number of car cylinders" using the function 
## lm(mpg ~ cyl, data=mtcars) and assign the resulted output to an object and name it model. Then show the output stored 
## within the object model.

model <- lm(mpg~cyl, data=mtcars)
model
## C) Show the summary result of the object model by using function summary(model). In addition, show the type of the 
## object summary(model) ### Hint: you can use the function  typeof 

summary(model)

typeof(summary(model))

## D) Extract and show the data stored in the item named coefficients from the summary(model) list. 

summary(model$coefficients)

## E) Write your own function to extract the coefficient estimate, standard error and the p-value for the variable x from any
#regression model x~y. Call the function beta_se. You have to use the regression model object as the input of this function.
# The new function should extract a vector of length three with the required results: coefficient estimate; standard error 
# the pvalue 

# to explore what items can be extracted from the summary list.
names(summary(model))

#my own function 
beta_se <- function(my_model){
  summary.model <- summary(my_model)
  coef <- summary.model$coefficients 
  output <- coef[2, c("Estimate", "Std. Error", "Pr(>|t|)")]
  return(output)
}
beta_se(model)

## F) Use the beta_se function to extract the effect estimates, standard errors and p values of the regression model of:
## 1) "Displacement" on "Miles per gallon"
## 2) "Rear axle ratio" on "miles per gallon"

model1 <- lm(mpg~disp, mtcars)
beta_se(model1)

model2 <- lm(mpg~drat, mtcars)
beta_se(model2)
