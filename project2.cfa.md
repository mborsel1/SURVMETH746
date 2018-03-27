####### DATA MANAGEMENT #######
#setwd( "C:/R for Class/746/Paper 2" )
####load data
#library(haven)
#GSS14 <- read_sav("C:/R for Class/746/Paper 2/GSS2014merged_R7.sav")
#View(GSS14)
#
#save( GSS14 , file = "GSS14.rda" )
#
#load("GSS14.rda")
#######

#### LIBRARIES ####
library(lavaan)
library(semPlot)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(xlsx)

####### ONE LATENT VARIABLE MODEL #######
# Set up model string for fit
CFAmodel1 <- '
civaware =~ poleff19 + news + discpol + signdpet + avoidbuy + attrally
'
# Fit the model
fit1 <- cfa(CFAmodel1, data=GSS14, estimator = "WLS", se = "bootstrap")

# Get summary and plot SEM diagram
summary(fit1, fit.measures=TRUE)
semPaths(fit1)

####### TWO LATENT VARIABLE MODEL #######
# Set up model string for fit
CFAmodel2 <- '
civaware =~ poleff19 + news + discpol
polact =~ signdpet + avoidbuy + attrally

#direct effect
polact ~~ civaware
'
# Fit the model
fit2 <- cfa(CFAmodel2, data=GSS14, estimator = "WLS", se = "bootstrap")

# Get summary and plot SEM diagram
summary(fit2, fit.measures=TRUE)
semPaths(fit2)

####### MODEL COMPARISON #######

anova(fit1,fit2)

####### OUTPUT TABLES #######

# Export summaries
sink("1_Var.txt")
summary(fit1, fit.measures=TRUE) 
sink() 

sink("2_Var.txt")
summary(fit2, fit.measures=TRUE) 
sink()     

# Excel export fit measures and loadings
write.xlsx(fitMeasures(fit1), "paper2export.xlsx", sheetName="1 Var Measures", append=FALSE)
write.xlsx(parameterEstimates(fit2), "paper2export.xlsx", sheetName="1 Var Loadings", append=TRUE)
write.xlsx(fitMeasures(fit2), "paper2export.xlsx", sheetName="2 Var Measures", append=TRUE)
write.xlsx(parameterEstimates(fit2), "paper2export.xlsx", sheetName="2 Var Loadings", append=TRUE)

####### PLOTS ########

# Create new dataset for setting up data for plots

desc <- GSS14[,c("poleff19","news","discpol","signdpet","avoidbuy","attrally")]

# Format variables as factors with correct labels for plots

desc$poleff19f<-factor(desc$poleff19,levels=c(1,2,3,4,5),
                      labels=c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
desc$newsf<-factor(desc$news,levels=c(1,2,3,4),
                  labels=c("Every day","A few times a week","Less than once a week","Never"))
desc$discpolf<-factor(desc$discpol,levels=c(1,2,3,4),
                     labels=c("Often","Sometimes","Rarely","Never"))

desc$signdpetf<-factor(desc$signdpet,levels=c(1,2,3,4),
                      labels=c("In past year","More distant past","Have not but might","Have not and would never"))
desc$avoidbuyf<-factor(desc$avoidbuy,levels=c(1,2,3,4),
                      labels=c("In past year","More distant past","Have not but might","Have not and would never"))
desc$attrallyf<-factor(desc$attrally,levels=c(1,2,3,4),
                      labels=c("In past year","More distant past","Have not but might","Have not and would never"))

### PLOT POLITCAL AWARENESS OBSERVED VARS ###

freq <- table(col(desc[,4:6]), as.matrix(desc[,4:6]))

#Then you need to create a data frame out of it, melt it and plot it:

Variable=c("Signed a petition","Boycotted a product","Attended a rally")     # create list of names
desc.2=data.frame(Variable, cbind(freq))   # combine them into a data frame

# Melting for plotting and setting up labels
desc.2m <- melt(desc.2, id="Variable")

levels(desc.2m$variable)[1] <- "In past year"
levels(desc.2m$variable)[2] <- "More distant past"
levels(desc.2m$variable)[3] <- "Have not but might"
levels(desc.2m$variable)[4] <- "Have not and would never"

colnames(desc.2m) <- c("Variable","Response","Count")

# Save plot for arranged output
pl1 <- ggplot(desc.2m, aes(Variable, Count)) +   
 geom_bar(aes(fill = Response), position = "dodge", stat="identity") + scale_fill_grey() + 
 scale_y_continuous( limits = c(0,700), expand = c(0,0) ) +
 labs(title="Political activism",x="Variable",y="Count")

### PLOT CIVIC AWARENESS OBSERVED VARS ###

# Save plots for arranged output
pl2 <- ggplot(data=subset(desc, !is.na(poleff19f)), aes(poleff19f)) + geom_bar() + 
 scale_y_continuous( limits = c(0,700), expand = c(0,0) ) +
 labs(title="Civic awareness",x="Understanding of political issues",y="Count")
pl3 <- ggplot(data=subset(desc, !is.na(newsf)), aes(newsf)) + geom_bar() + 
 scale_y_continuous( limits = c(0,700), expand = c(0,0) ) +
 labs(title=NULL,x="Frequency of reading the newspaper",y="Count")
pl4 <- ggplot(data=subset(desc, !is.na(discpolf)), aes(discpolf)) + geom_bar() + 
 scale_y_continuous( limits = c(0,700), expand = c(0,0) ) +
 labs(title=NULL,x="Frequency of discussing politics",y="Count")

### GENERATE PLOTS ###

grid.arrange(pl1, pl2, pl3, pl4, nrow = 4)
