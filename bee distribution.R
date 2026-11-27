dat = read.csv("Eulaema.csv")

library(MASS)

# Calculate the mean center variables
dat$mcMAT = dat$MAT - mean(dat$MAT, na.rm=T)
dat$mcMAP = dat$MAP - mean(dat$MAP, na.rm=T)
dat$mcTseason = dat$Tseason - mean(dat$Tseason, na.rm=T)
dat$mcPseason = dat$Pseason - mean(dat$Pseason, na.rm=T)
dat$mcforest = dat$forest. - mean(dat$forest., na.rm=T)
dat$mclu_het = dat$lu_het - mean(dat$lu_het, na.rm=T)
dat$mcaltitude = dat$altitude - mean(dat$altitude, na.rm=T)

# Evaluate correlation
vars <- dat[, c("mcMAT","mcMAP", "mcTseason", "mcPseason", "mcforest", "mclu_het", "mcaltitude")]
cor(vars, use = "complete.obs")

# Evaluate different negative binomial generalized linear models
m0 = glm.nb(Eulaema_nigrita ~ mcMAT + mcMAP + mcTseason + mcPseason +
              mcforest + mclu_het + mcaltitude + offset(effort),
              data = dat) # With all the predictor variables

m1 = glm.nb(Eulaema_nigrita ~ mcMAT + mcMAP + mcTseason + mcPseason + 
              mcforest + offset(effort),
              data = dat) # Without mcaltitude because it had strong correlation with mcMAT

m2 = glm.nb(Eulaema_nigrita ~ mcMAP + mcTseason + mcPseason + mcforest + 
              mcaltitude + offset(effort), 
              data = dat) # Without mcMAT because it had strong correlation with mcaltitude

m = glm.nb(Eulaema_nigrita ~ mcMAP + mcTseason + mcPseason + mcforest + offset(effort),
             data = dat)

# Calculate AIC values
mlist = list(m0, m1, m2, m)
AICTab = AIC(m0, m1, m2, m)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

summary(m)

# Plot the response of E. nigrita to Tseason, while also representing the effect of mcMAP
plot(dat$mcTseason, dat$Eulaema_nigrita, col="grey", las=1,
     xlab="Temperature seasonality (°C × 100)",
     ylab=expression(paste(italic("Eulaema nigrita")," abundance")))

newTseason = seq(min(dat$mcTseason), max(dat$mcTseason), length.out=200)
newMAP = rep(mean(dat$mcMAP), length(newTseason))
newPseason = rep(mean(dat$mcPseason), length(newTseason))
newforest = rep(mean(dat$mcforest), length(newTseason))
neweffort = rep(mean(dat$effort), length(newTseason))

y_hat = predict(m, newdata=list(mcMAP=newMAP,
                                mcPseason=newPseason,
                                effort=neweffort,
                                mcforest=newforest,
                                mcTseason=newTseason), 
                type="response")

lines(newTseason, y_hat, lwd=2)

newMAP2 = rep(mean(dat$mcMAP)+sd(dat$mcMAP), length(newTseason))
y_hat2 = predict(m, newdata=list(mcMAP=newMAP2,
                                 mcPseason=newPseason,
                                 effort=neweffort,
                                 mcforest=newforest,
                                 mcTseason=newTseason), 
                 type="response")

newMAP3 = rep(mean(dat$mcMAP)-sd(dat$mcMAP), length(newTseason))
y_hat3 = predict(m, newdata=list(mcMAP=newMAP3,
                                 mcPseason=newPseason,
                                 effort=neweffort,
                                 mcforest=newforest,
                                 mcTseason=newTseason), 
                 type="response")

lines(newTseason, y_hat2, lwd=2, col="blue3")
lines(newTseason, y_hat3, lwd=2, col="firebrick")

legend("topleft", lty=1, lwd=2, col=c(1, "blue3", "firebrick"), bty="n", 
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))

# Calculate predicted abundance at minimum and maximum Tseason
coefs = summary(m)$coefficients
# Minimum Tseason
exp(coefs[1,1] +
      coefs[3,1] * min(dat$mcTseason) +
      mean(dat$effort))

# Maximum Tseason
exp(coefs[1,1] +
      coefs[3,1] * max(dat$mcTseason) +
      mean(dat$effort))
