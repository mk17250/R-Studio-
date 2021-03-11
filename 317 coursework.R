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
lm(formula = y ~ x)
relation <- lm(y~x)
print(summary(relation))
plot(x,y)
abline(lm(y ~ x), col = "blue")
