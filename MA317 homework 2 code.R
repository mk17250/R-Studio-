########################################### Homework 2 for 317 #####################################################

### Load Data ###
Data1 <- read.csv("C:/Users/Home/OneDrive/practical_w3/LifeExpectancyData1 .csv", header=TRUE)
Data1 
class(Data1)

### LOad relevant libraries ###
install.packages("DataExplorer")
library(DataExplorer)
require(DataExplorer)
library(dsEssex)
require(dsEssex)
require(dplyr)  
require(tidyverse)
library(caTools)  
require(caTools) 
library(ISLR)
require(ISLR)
library(ggplot2)
require(ggplot2)
library(corrr)
library(cluster)
install.packages("corrplot")
library(corrplot)

### Analyise data ###
## Having observed the data I believe the country code column is surplus and therefore can be dropped: 
Data1 <- subset(Data1, select = -c(Country.Code))
Data1
## See names of variables:
orginal_names <- names(Data1)
orginal_names
## Change variable names from codes to names:
names(Data1) <- c("Country_Name", "Life_expectancy_at_birth_(years)", "Access_to_electricity_(percent_pop)",
                "Adjusted_net_national_income_(annual_percent)", "Adjusted_net_national_income_(Constant_2010_US)",
                "Children_out_of_school_(percent_Primary_age)", "Expenditure_on_primary_education_(percent)", 
                "Mortality_rate_(Infant)", "Literacy_rate_(adult_percent)", "Population_Growth_(percent)", 
                "Population_(total)", "Primary_completion_rate_(total_percent)", 
                "Current_health_expenditure_(percent_GDP)", "Current_health_expenditure_(per_capita)", 
                "Unemployment_(percent)", "Morality_rate_(adult_female_per_1000)", 
                "Mortality_rate_(adult_male_per_1000)", "GDP_(percent)", "GDP_(per_capita)", 
                "Birth_rate_(per_1000)", "GNI_(per_capita)", "Employment_to_pop_ratio_(percent)")


## look at size of dataframe (rows, cols)
size <- c(nrow(Data1), ncol(Data1))
size
## look at number of nans in each column
summary(Data1)
## Due to 209 observations out of 232 for expenditure on education, drop this column
Data1 <- subset(Data1, select = -c(`Expenditure_on_primary_education_(percent)`, `Literacy_rate_(adult_percent)`))
###should probably drop row 67 as target variable is missing 
########################## USE DATA IMPUTATION HERE ############################################
install.packages("mice")
library(mice)

### Help visualize nans ###
md.pattern(Data1) #################### THIS NEEDS NEATING UP ON THE TITLE/AXIA TO BE USED!!!!!!!!!!!!!!!!

## create dataset without country name 
Data2 <- subset(Data1, select = -c(`Country_Name`))
## impute data using mice, this imputes random values in place of the NANS from the distribution of the data

imputations <- mice(Data2, seed=50, m=1)

imputations$imp

### Create object without target variable and country name to use for correlation matrix ###
Data_corr <- subset(Data1, select = -c(`Life expectancy at birth (years)`, `Country Name`))

### Create correlation Matrix ###
plot_correlation(Data_corr, type = "continuous")

                