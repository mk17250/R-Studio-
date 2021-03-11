x <- c(183,171,190,192,179,168,175,190)
y <- c(58,72,66,48,45,85,63,35)
y_bar <- mean(y)
x_bar <- mean(x)
n <- length(x)
s.xx <- (sum(x**2) - n * (mean(x))**2)
s.xx
s.xy <- (sum(x*y) - n * (mean(x) * mean(y)))
s.xy
b <- s.xy / s.xx                                                                 # b = Beta 
a <- mean(y) - b * mean(x)                                                       # a = Alpha 
lmodel1 <- lm(y ~ x)
relation <- lm(y~x)
print(summary(relation))
plot(x,y)
abline(lm(y ~ x), col = "blue")
y_hat <- a+(b*x)
SSr <- sum((y-y_hat)**2)
std_res_r = SSr/6
k <- 1 
sigma_sqr_hat <- SSr/(n-k-1)
sxxx <- sum((x-x_bar)**2)

ressid <- relation$residuals
ressid_sqrd <- ressid**2.
SSR_ressid <- sum(ressid_sqrd)
sigma <- SSR_ressid/6
ressid_stand <- rstandard(relation)
ressid_stand
## Intercept of multivariate model1

?rstandard
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