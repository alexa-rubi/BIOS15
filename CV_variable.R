# Use non-parametric bootstrapping to derive a 95% confidence interval for the CV of a variable

CV <- function (x){

result <- NULL

for (i in 1:1000){
  # Sampling with replacement
  sample1 <- sample (x, replace = TRUE)
  # Calculate CV
  result[i] <- (sd(sample1) / mean(sample1))
}

# Calculate the 95% confidence interval
CI <- quantile(result, c(0.025, 0.975))

return(CI)

}

set.seed(1)
variable <- rnorm(50, 10, 2)

print(CV(variable))