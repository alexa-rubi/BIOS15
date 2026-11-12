birds = read.csv("~/Bioinformatics/BIOS15/BIOS15/bird_allometry.csv")
head(birds)

# Divide the data into males and females
males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]

# Fit linear models to log-transformed brain and body sizes
mm = lm(log(brain_mass)~log(body_mass), data=males)
mf = lm(log(brain_mass)~log(body_mass), data=females)

(summary(mm))
(summary(mf))

# Scatterplot for males
plot(log(brain_mass)~log(body_mass), data=males,
     main="Brain mass vs Body mass in Males",
     xlab="Brain mass (log g)",
     ylab="Brain mass (log g)",
     pch = 21, bg = "#87CEFA", col = "black",
     lwd = 0.9, cex = 1)

abline(mm, lwd=2)

# Scatterplot for females
plot(log(brain_mass)~log(body_mass), data=females,
     main="Brain mass vs Body mass in Females",
     xlab="Body mass (log g)",
     ylab="Brain mass (log g)",
     pch = 21, bg = "firebrick", col = "black",
     lwd = 0.9, cex = 1)

abline(mf, lwd=2)

# Scatterplot for both
plot(log(brain_mass) ~ log(body_mass), data=males,
     pch = 21, bg = "#87CEFA", col = "black",
     lwd = 0.9, cex = 1,
     xlab="Body mass (log g)",
     ylab="Brain mass (log g)",
     main="Brain mass vs Body mass")

points(log(brain_mass) ~ log(body_mass),
       data=females, pch = 21, bg = "firebrick", col = "black",
       lwd = 0.9, cex = 1)

abline(mm, col="black", lwd=2)
abline(mf, col="black", lwd=2)

legend("bottomright",
       legend=c("Males", "Females"),
       col=c("#87CEFA", "firebrick"),
       pch=16)