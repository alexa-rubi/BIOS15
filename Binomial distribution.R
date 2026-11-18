# Binomial distribution

x = seq(from=0, to=1, by=0.01)
v_b = x*(1-x) # Binomial variance
plot(x,v_b,
     type="l",
     xlab="Probability",
     ylab="Theorical variance",
     las=1)

# Transform the proportional data in a way that makes them approach a normal distribution
# For this we can use the logit or log odds transformation

logit = function(x) log(x/(1-x)) # Formula
invlogit = function(x) 1/(1+exp(-x))

x = runif(200)
logit_x = logit(x)

par(mfrow = c(2,2))
hist(x, las=1)
hist(logit_x, las=1)

xx = seq(-5, 5, 0.01)
plot(xx, invlogit(xx),
     type="l",
     las=1,
     xlab= "Logit (x)",
     ylab= "P")

plot(x,invlogit(logit_x), las=1)

# A similar alternative is the so-called probit link, which corresponds to the quantile 
# distribution of the standard normal distribution 

plot(xx, invlogit(xx),
     type="l",
     las=1,
     xlab= "Logit/Probit (X)",
     ylab= "P")
lines(xx,pnorm(xx), lty=2)
legend("topleft",legend=c("Logit", "Probit"),
       lty=c(1,2), bty="n")
