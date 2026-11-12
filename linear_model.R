set.seed(85)
x = rnorm(n=200, mean=10, sd=2)
y = 0.4*x + rnorm(200, 0, 1)

# Fit linear model
m = lm(y~x)

# Extract the model coefficients (parameter estimates)
cf = m$coef

# Obtain the predicted values
predvals = cf[1] + cf [2]*x

# Compute predicted values with the equation for the linear model
par(mfrow=c(1,2))
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

# Add line segments to illustrate residuals
abline(m)
segments(x, y, x, predvals)
hist(residuals(m), xlab="", las=1)

# Keep the regression line within the data range. For this,
# make predictions for new values of x spanning the data range
newx = seq(min(x), max(x), length.out=200)
predy = cf[1] + cf [2]* newx
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

# Add the regression line using the lines function
lines(newx,predy)

# Regression slope
cov(y,x)/var(x)

# How much y changes for a one standard deviation change in x
(cf[2]*(mean(x) + sd(x))) - (cf[2]*mean(x))

# Coefficient of determination (R^2)
cor(x,y)^2
