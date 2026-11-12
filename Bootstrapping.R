# Bootstrap simulation. It repeatedly takes random samples from the original data (x), 
# calculates their means, and stores those means

# Sets the random number seed. Makes the results reproducible
set.seed(1)

# Generates 50 random numbers from a normal distribution.
# 50 values, mean of 10 and standard deviation of 2
x <- rnorm(50,10,2)

# Calculate the standard error of the mean
se_x <- sqrt(var(x)/length(x))

# Create an empty object to store the means calculated in each iteration of the loop
out <- NULL

# The bootstrap. Repeats the instructions inside the loop 1000 times
for ( i in 1:1000){
  
  # Takes a random sample from the original data x
  sample1 <- sample (x, replace=TRUE) # Sampling with replacement
  
  # Calculates the mean of the new sample and stores it in out
  out[i] <- mean(sample1)
}

hist(out, las=1, main="")

# Calculate a 95% bootstrap confidence interval
quantile(out, c(0.025, 0.975))