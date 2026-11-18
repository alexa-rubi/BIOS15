# Logistic regression

# We simulate data by first formulating a linear predictor η 
x = rnorm(200, 10, 3)
eta = -2 + 0.4*x + rnorm(200, 0, 2)

# Then transforming the predicted values into probabilities 
# (through the inverse-logit transformation)
p = invlogit(eta)

# Finally binarizing the data by sampling from the binomial distribution
y = rbinom(200, 1, p)

par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, las=1)

# To fit the generalized linear model, we use the glm function and specify 
# the error family (binomial) and link function (logit)
m = glm(y~x, family = binomial(link = "logit"))
summary(m)

coefs = summary(m)$coef

# To produce a regression line, we define some new x-values that spans
# the data range along the x-axis
x_pred = seq(from=min(x), to=max(x), by=0.01)

# Then obtain predicted values ˆy (using the model coefficients)
y_hat = coefs[1,1] + coefs[2,1]*x_pred

# Transform to the probability scale to obtain the predicted probabilities ˆp.
p_hat = invlogit(y_hat)

# Plot the original data
plot(x, y, 
     las=1,
     xlab="x",
     ylab="y")

# Add the binomial regression curve
lines(x_pred, p_hat, lwd=2)

# Compute the value of x corresponding to a probability of 0.5
abline(h=0.5, lty=2)
abline(v=-coefs[1,1]/coefs[2,1], lty=2)


