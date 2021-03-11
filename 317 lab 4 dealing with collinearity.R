#### LAB 4 for 317 #####

#### Dealing with Collinearity ##################

## load package and library
install.packages('faraway')
library('faraway')

##load datset ###
data("seatpos")
data <- seatpos

### quick view of data set, check for nans ###
summary(data)
a = sum(is.na(data))
a

### Create linear model including all possible explanatory variables ###
model1 <- lm(hipcenter ~ ., data)
summary(model1)
## looking at the F statistic and the p-value, the F stat is fairly high and the p value is small, you might be 
## tempted to infer that our model fits our dataset well, but the explanatory variables are all not statistically 
## significant at most critical values. What is going on here? 

### checking for collinearity ###
## drop the dependant varaible from the dataset
X <- data[-9]
X
## create correlation matrix 
corr <- cor(X)
corr
## visualize 
plot_correlation(corr, type = "continuous")
## or
install.packages("corrplot")
library(corrplot)
corrplot.mixed(corr, lower.col = "black", number.cex = .7)

### How to ascertain which explanatory variables to keep in the model and which to disgaurd? ###
## We can use the VIF (Variance Inflation Factor)
vif(X)
## when the VIF values are this indicates high collinearity with the other variables 

### We can now experiment with variables, excluding ones with collinearity to find a best fitting model 
model2 <- lm(hipcenter ~ Age , data = data)
summary(model2)
model3 <- lm(hipcenter ~ Weight, data)
summary(model3)
model4 <- lm(hipcenter ~ Ht, data)
summary(model4)
model5 <- lm(hipcenter ~ Seated, data)
summary(model5)
model6 <- lm(hipcenter ~ Leg, data)
summary(model6)
model7 <- lm(hipcenter ~ Ht + Age, data)
summary(model7)
## Having experimented with models that have uncorrelated explanatory variables in we can see that hipcenter 
## is best explained by height, we can use ANOVA to test this against the only other multivariate model 
## without correlated explanatory variable in , Height and Age to see which is superior 
anova(model4, model7)
## or similarly for the model with all variable comapred to just Height 
anova(model4, model1)
## we already can tell that we fail to reject the null for Age from the lm and the ANOVA further confirms 
## that Height is the more accurate predictor variable for hipcentre. 

############################################# END ###############################################################