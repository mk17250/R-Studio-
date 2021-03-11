install.packages("rtools")
install.packages("drat")
drat::addRepo("statcourses")
install.packages("dsEssex")
require(dsEssex)

###################################### EXERCISE 1 #############################################################

# a) create a working folder named 'practical-w3' at a path of your choice on your machine. Set you working 
#directory to the created practical_w3 folder. Then, download the data set file named lbw.csv from moodle and 
#save it to your working directory, practical_w3:


setwd("C:/Users/Home/OneDrive/practical_w3")

lbw <- read.csv("lbw.csv", header = T)

rm(list = ls())
data("lbw") # call lbw data
head(lbw) # show first 5 lines of lbw data set 

?lbw # this displays the description of the data 

names(lbw) #shows variable names in lbw 

# extract data for the first 180 subjects with only the following variables: id, maternal age at delivery,
# maternal smoking, birth weigh. Assign the subset to object named Reduced.lbw

(Reduced.lbw <- lbw[1:180, c(1,3,6,11)])

(Reduced.lbw$yage <- Reduced.lbw$age < 19) ##$ is important and assigns it to it 
# this creates and adds a variable to the Reduced.lbw object
#with mothers aged below 19 - PRINT TO VIEW 
Reduced.lbw
# f) In order to establish which mothers identified as young smokers we:
sum(Reduced.lbw$yage & Reduced.lbw$smoke)

# g) Find out how many mothers identified as young OR smoker;
sum(Reduced.lbw$yage | Reduced.lbw$smoke)

#h) Find out how many non-smokers are mothers:
sum(!Reduced.lbw$smoke)
# same as:
sum(Reduced.lbw$smoke != 1) #### NOTE != EQUALS NOTE EQUAL TOO. SAME ABOVE. WHENEVER YOU SEE !. not equal to.

# i) Write down the names of the statistics or the role that each of the following functions calculates. 
#Hint: you can look at the help page of a function using '?<function-name>'

attach(Reduced.lbw)
mean(bwt)

?attach # attach set of R objects to search path:The database is attached to the R search path. This means
#dat the database is searched by R when evaluating a variable, so objects in the database can be accessed by
#simply giving their names.

?mean # arithmetic mean

?sd #Standard deviation 
#This function computes the standard deviation of the values in x
#If na.rm is TRUE then missing values are removed before computation proceeds.

?min #minimum values 

?max #max value

?median #computes sample median 

?IQR#Calculates the inter quartile range 

?range #returns a vector containing the from the minimum to the maximum of all given arguments 

?summary #summary is a generic function used to produce result summaries of the results of various model fitting functions
# The function invokes particular methods which depend on the class of the first argument.

?detach #Detach a database, i.e., remove it from the search() path of available R objects.
#sually  is either a data.frame which has been attached or a package which was attached by library.


#j) Use one or more functions mentioned in the previos question to summarise the birth weight for smoker mothers
#and non-smoker mothers:
attach(Reduced.lbw)
by(data = bwt, INDICES = smoke, FUN = summary)

#k) Calculate the correlation coefficient between ( age and bwt ) HINT: Use the help function for cor function
#to find out what it is and how it works 
?cor

cor(Reduced.lbw$age, Reduced.lbw$bwt)

############################### EXERCISE 2 ####################################

#a) Generate a vector called 'rand1' consisting of 100 normally distributed values simulated with a mean of 11
#and standard deviation of 5.5. Look for help for the rnorm function and make use of it to achieve this.
?rnorm 
rand1 <- rnorm(100, 11, 5.5)

#b) Re-execute the generation of the same values as in the question Ex2.a but name the vector rand2 this time. 
rand2 <- rnorm(100,11, 5.5)

#c) Are rand1 and rand2 the same? You use the function 'identical' to check this:
identical(rand1, rand2)

#d) Now use 'set.seed(57375)' command before generating the rand1 vector and again before generating the 
#rand2 vector. When you return them all, are rand1 and rand2 now the same?! What do you think the command 
#'set.seed(57375)' did her?  

set.seed(57375)
rand1 <- rnorm(100, 11, 5.5)
set.seed(57375)
rand2 <- rnorm(100, 11, 5.5)
identical(rand1, rand2)
####### THE SET.SEED COMMAND ENSURE THAT THE GENERATED VECTOR IS REPRODUCABLE (IE REPEATED EXECUTION OF YOUR
#SCRIPT SHOULD LEAD TO AN IDENTICAL VECTOR)

#e) Which elements (if any) of the rand1 vector are less than the first quartile of its distribution and positive.
#Hint: The first quartile of a distribution is the value lies at the first quarter of the data after 
#ordering the values ascendingly. The function quantile gives the values of the distribution at any position 
#of your choice after ordering the data, see ?quantile



############################################  END  ############################################################