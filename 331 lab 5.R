##### 331 Lab 5 ########

##### Exercise 1 ########

## Create a function 'alt_plot' that takes two numeric vectors, x and y and plots the difference between x-y on the y axis
## and the sum of x+y on the x axis. Draw two random vectors x and y with a length of 150 from a standard normal and call 
## function 

alt_plot <- function(x,y){
  y <- x-y
  x <- x+y
  plot <- plot(x,y, main = "Relationship between difference of x-y by sum of x-y", xlab = "sum of x+y", ylab = "differnce of x-y")
  abline(lm(y~x), col ="blue")
  return(plot)
}
x <- rnorm(150)
y <- rnorm(150)

alt_plot(x,y)

##### Exercise 2 #######

## Wrtite a function 'comp_ss_n' that for a given n computes the sum SSn = 1^2 + 2^2+... +n^2. Call function with n=12

comp_ss_n <- function(n){
  SSn <- sum(seq(1,n)**2)
  return(SSn)
}

comp_ss_n(12)

####### Exercise 3 ########

### Define a numerical vector ss_n of size 35 using ss_n <- vector(mode = "numeric",length = 35) and store the results of
## SS1, SS2.... SS35 using a for loop. 

### for loop ###
#### create empty numerical vector ####
ss_n <- vector(mode = "numeric", length = 35)

for (i in seq(1,35)){
  ss_n[i] <- comp_ss_n(i)
}
ss_n

##### Exercise 4 ######

## Repeat exercise 3, but this time use the sapply function.

sapply(seq(1,35), comp_ss_n)

###### Exercise 5 ######

### Calculate means of the ss_n for the even numbers of n and the odd numbers for the n vector ss_n using the tapply function

## Define a logical vector with odds or even upto 35
even_or_odd <- ifelse(1:35 %% 2 == 0, "Even", "Odd")
even_or_odd

#using tapply
tapply(ss_n, INDEX = even_or_odd, FUN = mean)
tapply(ss_n, even_or_odd, mean)

##################################### RECAPPING THE HORUS 5 #############################################################

#### LOOPS####### 
## for loop
##Here we defined this function previously, what if we wanted to implement this 100 times for a range of integers? 
## We would use a for loop
sum.n <- function(n){
  n * (n+1)/2
}
sum.n(3)

#for loop example:
n <- 3:35
l <- length(n)
#create empty vector with vector function
s_n <- vector(length = l)
#create loop
for (i in 1:l){
  s_n[i] <- sum.n(n[i])  #on the right handside of this function we are calling the sum.n function and inputing the n object
  #for the ith observations 
}
s_n

plot(n, s_n)

#### VECTORISATION ###########
#vectorised functions can be applied to vecxtors for example 
c <- 1:10
sqrt(c)
#sqrt is a vectorised function  

#### if as function is not vectorised then we must use a for loop to make it vectorised 
p <- 3:5 
sum.n(p)

## If we had define the function slightly differently it would not have worked
 # sum.n <- function(n){
  #x <- 1:n
  #sum(x)
#}
#this version of the function is not vectorised and will not accpet vectors. 

#### THE FUNCTIONALS #################################

##apply, sapply, tapply function can be used to vectorise a function 

# apply function 
# apply(X, MARGIN, FUN)
#X = matrix or dataframe 
#Margin = is 1 or 2. 1 = rows, 2=columns 
#Fun = function you wish to call .

##Lapply can operate on list and vectors as appose to matrices or dataframes

#create 3 matrices 
A <- matrix(rnorm(9), 3,3)
B <- matrix(rnorm(9), 3,3)
C <- matrix(rnorm(9), 3,3)
#create list containing matrices
LL <- list(A, B, C)
#use lappy to "[" = extract the ,2 (all rows, column 2) from the list of matrices 
lapply(LL, "[", ,2)


############################ SAPPLY #################################
## lapply returns a list where as sapply returns a different format - a vector see below once it has been run 

sapply(LL, "[", ,2)

## MAPPLY FUCNTION!!!!!!! THIS IS A GOOD ONE!!!! ##########################

#the mapply function will take a function that is not vectorised and apply it to each element making it a vectorised function 

###### tapply is another handy function ###############
