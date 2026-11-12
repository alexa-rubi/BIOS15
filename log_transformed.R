# Demonstrate that SD of log-transformed data is closely related to the CV of the 
# original data
 
set.seed(1)

# Different standard deviations for the normal distribution
sigma <- seq(0.05, 1, length.out = 20)

# Create empty matrix
results <- matrix(NA, nrow = 20, ncol= 2)

for (i in 1:20){
  # Generate normally distributed data and transform to log-normal
  x <- exp(rnorm(1000, 2, sigma[i]))
  
  # Calculate CV
  results[i, 1] <- (sd(x)/ mean(x))
  
  # Calculate log SD
  results[i, 2] <- sd(log(x))
}

# Plot the result 
plot(results[,2], results[,1],
xlab = "CV(x)",
ylab = "SD(log[x])",
main = "Similarity of the standard deviation of log-transformed data to the coefficient of variation",
pch = 1,
col ="hotpink"
)

# Add a reference line
abline(0, 1, lty = 2)