/************************************************************************
* Data Preparation - European Social Survey - SAS code
*************************************************************************/
libname a "C:\Users\zhxinyu\Desktop\746";
data a.ess; 
set int;
run;

########################################################################### 
# Life Satisfaction in European Social Survey - Multilevel Modeling in R
########################################################################### 

library(sas7bdat)
library(lme4)
library(lmerTest)

# data in
mydata <- read.sas7bdat("C:/Users/zhxinyu/Desktop/746/ess.sas7bdat") 

# 2014 gini index (for details please see ESS multilevel data)
mydata$gini[mydata$CNTRY == "AT"] <- 27.60
mydata$gini[mydata$CNTRY == "BE"] <- 25.90
mydata$gini[mydata$CNTRY == "CH"] <- 29.50
mydata$gini[mydata$CNTRY == "CZ"] <- 25.10
mydata$gini[mydata$CNTRY == "DE"] <- 30.70
mydata$gini[mydata$CNTRY == "DK"] <- 27.70
mydata$gini[mydata$CNTRY == "EE"] <- 35.60
mydata$gini[mydata$CNTRY == "ES"] <- 34.70
mydata$gini[mydata$CNTRY == "FI"] <- 25.60
mydata$gini[mydata$CNTRY == "FR"] <- 29.20
mydata$gini[mydata$CNTRY == "GB"] <- 31.60
mydata$gini[mydata$CNTRY == "HU"] <- 28.60
mydata$gini[mydata$CNTRY == "IE"] <- 31.10
mydata$gini[mydata$CNTRY == "IL"] <- NA
mydata$gini[mydata$CNTRY == "LT"] <- 35.00
mydata$gini[mydata$CNTRY == "NL"] <- 26.20
mydata$gini[mydata$CNTRY == "NO"] <- 23.50
mydata$gini[mydata$CNTRY == "PL"] <- 30.80
mydata$gini[mydata$CNTRY == "PT"] <- 34.50
mydata$gini[mydata$CNTRY == "SE"] <- 25.40
mydata$gini[mydata$CNTRY == "SI"] <- 25.00

# keep only cases with no missing on selected variables 
data <- mydata[, c("IDNO", "CNTRY", "STFLIFE", "EDUYRS", "gini")] 
newdata <- na.omit(data)

# specify model components: subject, predictors and dependent variable
CNTRY <- newdata$CNTRY 
STFLIFE <- newdata$STFLIFE  
iv.level1 <- newdata$EDUYRS 
iv.level2 <- newdata$gini

# descriptive statistics 
summary(STFLIFE)
sd(STFLIFE)
length(STFLIFE)
boxplot(STFLIFE)

summary(iv.level1)
sd(iv.level1)
length(iv.level1)
boxplot(iv.level1)

summary(iv.level2)
sd(iv.level2)
length(iv.level2)
boxplot(iv.level2)

# outliers in education years
# if EDUYRS > 35 then it will be imputed by the next value 
i.of.outliers <- which(newdata$EDUYRS > 35)
nexttoout <- i.of.outliers + 1

newdata[i.of.outliers,"EDUYRS"]<-newdata[nexttoout,"EDUYRS"]

summary(newdata$EDUYRS)
sd(newdata$EDUYRS)
length(newdata$EDUYRS)
boxplot(newdata$EDUYRS)
iv.level1 <- newdata$EDUYRS

# center numerical predictors
gini.country <- c(27.6, 25.9, 29.5, 25.1, 30.7, 27.7, 35.6, 34.7, 25.6, 29.2, 31.6, 28.6, 31.1, 35, 26.2, 23.5, 30.8, 34.5, 25.4, 25)
summary(gini.country)
sd(gini.country)
newdata$LEVEL1.EDUYRS.CENTERED  <- iv.level1 - mean(iv.level1, na.rm=TRUE)
newdata$LEVEL2.GINI.CENTERED <- iv.level2 - mean(gini.country)
LEVEL1.EDUYRS.CENTERED <- newdata$LEVEL1.EDUYRS.CENTERED
LEVEL2.EDUYRS.CENTERED <- newdata$LEVEL2.EDUYRS.CENTERED

# boxplot for life satisfaction
boxplot(STFLIFE ~ CNTRY, data = newdata, col = "lightgray",  xlab="County", ylab="Life Satisfaction")
means <- tapply(newdata$STFLIFE, newdata$CNTRY, mean)
points(means, col="red", pch = 18)
qqline(mean(newdata$STFLIFE), col = 2, lwd = 2, lty = 2)

# model 1: 
mod1 <- lmer(STFLIFE ~ 1 + (1|CNTRY), REML = T, data = newdata)
summary(mod1)
AIC(mod1)
BIC(mod1)

# test random intercept
rand(mod1)

# model diagnostic 
## EBLUPs intercept
qqnorm(ranef(mod1)$CNTRY[,1])
qqline(ranef(mod1)$CNTRY[,1])
## Residual
qqnorm(resid(mod1))
qqline(resid(mod1))
plot(resid(mod1), fitted(mod1))

# model 2: 
mod2 <- lmer(STFLIFE ~ LEVEL2.GINI.CENTERED + (1|CNTRY), REML = T, data = newdata)
summary (mod2)
AIC(mod2)
BIC(mod2)

# test random intercept
rand(mod2)

# model diagnostic 
## EBLUPs intercept
qqnorm(ranef(mod2)$CNTRY[,1])
qqline(ranef(mod2)$CNTRY[,1])
## Residual
qqnorm(resid(mod2))
qqline(resid(mod2))
plot(resid(mod2), fitted(mod2))

# model 3: 
mod3 <- lmer(STFLIFE ~ LEVEL1.EDUYRS.CENTERED + (1|CNTRY), REML = T, data = newdata)
summary (mod3)
AIC(mod3)
BIC(mod3)

# test random intercept
rand(mod3)

# model diagnostic 
## EBLUPs intercept
qqnorm(ranef(mod3)$CNTRY[,1])
qqline(ranef(mod3)$CNTRY[,1])
## Residual
qqnorm(resid(mod3))
qqline(resid(mod3))
plot(resid(mod3), fitted(mod3))

# model 4: 
mod4 <- lmer(STFLIFE ~ LEVEL2.GINI.CENTERED + LEVEL1.EDUYRS.CENTERED + (1|CNTRY), REML = T, data = newdata)
summary (mod4)
AIC(mod4)
BIC(mod4)

# test random intercept
rand(mod4)

# model diagnostic 
## EBLUPs intercept
qqnorm(ranef(mod4)$CNTRY[,1])
qqline(ranef(mod4)$CNTRY[,1])
## Residual
qqnorm(resid(mod4))
qqline(resid(mod4))
plot(resid(mod4), fitted(mod4))

# model 5: 
mod5 <- lmer(STFLIFE ~ LEVEL2.GINI.CENTERED + LEVEL1.EDUYRS.CENTERED + (LEVEL2.GINI.CENTERED | CNTRY), REML = T, data = newdata) 
summary (mod5)
AIC(mod5)
BIC(mod5)

# model diagnostic 
## EBLUPs intercept
qqnorm(ranef(mod5)$CNTRY[,1])
qqline(ranef(mod5)$CNTRY[,1])
## EBLUPs slope
qqnorm(ranef(mod5)$CNTRY[,2])
qqline(ranef(mod5)$CNTRY[,2])
## Residual
qqnorm(resid(mod5))
qqline(resid(mod5))
plot(resid(mod5), fitted(mod5))

# likelihood ratio test (Test for random coefficient) - REML: model 5 = 158152.6  model 4= 158153.1 
# null hypothesis: sigma1^2 = 0 ; df is the q correlated random effects vs q+1 correlated random effects 
TestStatistic <- 158153.1 - 158152.6
p <- 0.5 * (1 - pchisq(TestStatistic, 1)) + 0.5 * (1 - pchisq(TestStatistic, 2))
#  p = 0.6291505 > 0.05

############################################### 
#Attempt: logistic multilevel model
###############################################
library(ggplot2)
library(GGally)
library(reshape2)
library(lme4)
library(compiler)
library(parallel)
library(boot)
library(DHARMa) 
# details of DHARMa can be found here: 
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#installing-loading-and-citing-the-package

#logistic MLM recoding
newdata$STF1 [newdata$STFLIFE <= 5 ] <- 0 
newdata$STF1 [newdata$STFLIFE > 5 ] <- 1 

# logit model 1: 
logit.mod1 <- glmer (STF1 ~ 1 + (1 | CNTRY), family = binomial, data = newdata, nAGQ = 10)
print (logit.mod1, corr = FALSE)
dotplot(ranef(logit.mod1, which = "CNTRY", postVar = TRUE))
se1 <- sqrt(diag(vcov(logit.mod1)))
(tab1 <- cbind(Est = fixef(logit.mod1), LL = fixef(logit.mod1) - 1.96 * se1, UL = fixef(logit.mod1) + 1.96 * se1))
exp(tab1)

#ICC1: 0.1628717
r1Var <- as.numeric(VarCorr(logit.mod1)[["CNTRY"]])
residVar <- pi^2/3
ICC1 <- r1Var/(r1Var+residVar)

# model diagnostic 
simulationOutput1 <- simulateResiduals(fittedModel = logit.mod1, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput1)

# logit model 2: 
logit.mod2 <- glmer (STF1 ~ LEVEL2.GINI.CENTERED + (1 | CNTRY), family = binomial, data = newdata, nAGQ = 10)
print (logit.mod2, corr = FALSE)
se2 <- sqrt(diag(vcov(logit.mod2)))
(tab2 <- cbind(Est = fixef(logit.mod2), LL = fixef(logit.mod2) - 1.96 * se2, UL = fixef(logit.mod2) + 1.96 * se2))
exp(tab2)
dotplot(ranef(logit.mod2, which = "CNTRY", postVar = TRUE))

# model diagnostic 
simulationOutput2 <- simulateResiduals(fittedModel = logit.mod2, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput2)

# logit model 3: 
logit.mod3 <- glmer (STF1 ~ LEVEL1.EDUYRS.CENTERED + (1 | CNTRY), family = binomial, data = newdata, nAGQ = 10)
print (logit.mod3, corr = FALSE)
se3 <- sqrt(diag(vcov(logit.mod3)))
(tab3 <- cbind(Est = fixef(logit.mod3), LL = fixef(logit.mod3) - 1.96 * se3, UL = fixef(logit.mod3) + 1.96 * se3))
exp(tab3)
dotplot(ranef(logit.mod3, which = "CNTRY", postVar = TRUE))

# model diagnostic 
simulationOutput3 <- simulateResiduals(fittedModel = logit.mod3, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput3)

# logit model 4: 
logit.mod4 <- glmer (STF1 ~ LEVEL2.GINI.CENTERED + LEVEL1.EDUYRS.CENTERED + (1 | CNTRY), family = binomial, data = newdata, nAGQ = 10)
print (logit.mod4, corr = FALSE)
se <- sqrt(diag(vcov(logit.mod4)))
(tab <- cbind(Est = fixef(logit.mod4), LL = fixef(logit.mod4) - 1.96 * se, UL = fixef(logit.mod4) + 1.96 * se))
exp(tab)
dotplot(ranef(logit.mod4, which = "CNTRY", postVar = TRUE))

# model diagnostic 
simulationOutput4 <- simulateResiduals(fittedModel = logit.mod4, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput4)

# logit model 5: 
logit.mod5 <- glmer (STF1 ~ LEVEL2.GINI.CENTERED + LEVEL1.EDUYRS.CENTERED + (LEVEL2.GINI.CENTERED | CNTRY), family = binomial, data = newdata, nAGQ = 1)
print (logit.mod5, corr = FALSE)
se <- sqrt(diag(vcov(logit.mod5)))
(tab <- cbind(Est = fixef(logit.mod5), LL = fixef(logit.mod4) - 1.96 * se, UL = fixef(logit.mod5) + 1.96 * se))
exp(tab)
dotplot(ranef(logit.mod4, which = "CNTRY", postVar = TRUE))

# model diagnostic 
simulationOutput5 <- simulateResiduals(fittedModel = logit.mod5, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput5)


#I acknowledge that this is a poor mlm design due to the way we used allowed gini coefficients to "vary"
