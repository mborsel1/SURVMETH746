* Load Dataset
use "C:\Users\ariji\OneDrive\Documents\3rd Sem\Statistical Modelling\Assignment4\hrs_reduced2.dta"

* Summarize dependent variable by year for descriptive statistics
tab yrssince00,sum( totassets000)

* Summary of demographic variables
table MARCAT EDCAT RACECAT,contents(freq)

* Lowess Plot of outcome variables- first wrt MARCAT (1= Married; 2 = Previously Married; 3= Never Married)
twoway (lowess totassets000 year), by( MARCAT)

* Lowess Plot of outcome variable – by EDCAT; 1= Less than H.S. 2= High School.3 = Attended College. 4= College Graduate and above
twoway (lowess totassets000 year), by( EDCAT)

* Lowess Plot of outcome variable, by RACECAT; 1=Hispanic, 2=White, 3=Black, 4=Other
twoway (lowess totassets000 year), by( RACECAT)

* Here is a Lowessoutput plot without categorical restrictions
twoway( lowess totassets000 year)

* Here are the relevant Sphagetti Plots that uses all observations, but are less informative due to clutter.
twoway (line totassets000 year), by( MARCAT)
twoway (line totassets000 year), by( EDCAT)
twoway (line totassets000 year), by( RACECAT)
twoway (line totassets000 year)

* Model fitting: our first objective is to find the best possible overall model for the trend in growth of assets for each demographic category (Marital Status, Education, and Race)

* We fit the model using correlated errors using an unstructured variance-covariance matrix but we do not care about random effects as we are not interested in trends for a given household
mixed totassets000 ib2.MARCAT ib1.EDCAT ib1.RACECAT ib0.yrssince00 ib2.MARCAT#ib0.yrssince00 ib1.EDCAT#ib0.yrssince00 ib1.RACECAT#ib0.yrssince00|| HHIDPN: , noconst residuals(unstructured,t(year)) variance reml
 
* Mimicking the Type-III tests we find in SAS to check for overall importance of fixed effects (note that the testing specification is different, Stata runs Wald Chi-square test, and SAS runs a F-test)

test 2.yrssince00 4.yrssince00 6.yrssince00

* Now for marital status
test 1.MARCAT 3.MARCAT

* Now for education
test 2.EDCAT 3.EDCAT 4.EDCAT

* Finally, RACECAT
test 2.RACECAT 3.RACECAT 4.RACECAT

* Next we test for the interaction terms – First, Year and Marital status
test 2.yrssince00#1.MARCAT 2.yrssince00#3.MARCAT 4.yrssince00#1.MARCAT 4.yrssince00#3.MARCAT 6.yrssince00#1.MARCAT 6.yrssince00#3.MARCAT

* Now Year and Education interaction
test 2.yrssince00#2.EDCAT 2.yrssince00#3.EDCAT 2.yrssince00#4.EDCAT 4.yrssince00#2.EDCAT 4.yrssince00#3.EDCAT 4.yrssince00#4.EDCAT 6.yrssince00#2.EDCAT 6.yrssince00#3.EDCAT 6.yrssince00#4.EDCAT

* Now Year and Race interaction
test 2.yrssince00#2.RACECAT 2.yrssince00#3.RACECAT 2.yrssince00#4.RACECAT 4.yrssince00#2.RACECAT 4.yrssince00#3.RACECAT 4.yrssince00#4.RACECAT 6.yrssince00#2.RACECAT 6.yrssince00#3.RACECAT 6.yrssince00#4.RACECAT

* Calculation of residuals and generating Q-Q Plot
predict resid, residuals
qnorm resid

* Now we define a top 25% group and a bottom 75% group. First we need to figure out the quartile values
summarize totassets000, detail

* Clearly the 75% value is 384. So we create binary variable which takes the value 1 if asset value is above 384 and takes the value 0 if asset value is below or equal to 384.
gen top25=0
replace top25=1 if totassets000>384
* To meet our second research objective – i.e. estimating whether differences being in the TOP 25 for particular demographic groups are changing over time, we estimate a GEE model suited for binary outcome

* First we have to plot subgroup proportions for the top quartiles. This is done way easily in R compared to Stata

# Loading Stata file
library(haven)
hrs_reduced2 <- read_dta("hrs_reduced2.dta")
# Generating aggregated datafiles, so we get proportions for each wave and 
demographic category
aggdata <- aggregate(hrs_reduced2$top25, by=list(hrs_reduced2$EDCAT,hrs_reduced2$year), FUN=mean, na.rm=TRUE)
colnames(aggdata) <- c("edcat","year","prop")
aggdata2 <- aggregate(hrs_reduced2$top25, by=list(hrs_reduced2$MARCAT,hrs_reduced2$year), FUN=mean, na.rm=TRUE)
colnames(aggdata2) <- c("marcat","year","prop")
aggdata3 <- aggregate(hrs_reduced2$top25, by=list(hrs_reduced2$RACECAT,hrs_reduced2$year), FUN=mean, na.rm=TRUE)
colnames(aggdata3) <- c("racecat","year","prop")

* Back to Stata. First we have to declare the dataset as panel
xtset HHIDPN yrssince00, delta(2)

* Now we run the GEE specification
xtgee top25 ib2.MARCAT ib1.EDCAT ib1.RACECAT yrssince00 ib2.MARCAT#c.yrssince00 ib1.EDCAT#c.yrssince00 ib1.RACECAT#c.yrssince00, family (binomial 1) link (logit) corr(unstructured)

* Results from Wald tests
test 1.MARCAT 3.MARCAT
test 2.EDCAT 3.EDCAT 4.EDCAT
test 2.RACECAT 3.RACECAT 4.RACECAT
test c.yrssince00#1.MARCAT c.yrssince00#3.MARCAT 
test c.yrssince00#2.EDCAT c.yrssince00#3.EDCAT c.yrssince00#4.EDCAT
test c.yrssince00#2.RACECAT c.yrssince00#3.RACECAT c.yrssince00#4.RACECAT

* Now we look at the working correlation matrix
estat wcorrelation

* We fit the model using a AR1 structure
xtgee top25 ib2.MARCAT ib1.EDCAT ib1.RACECAT yrssince00 ib2.MARCAT#c.yrssince00 ib1.EDCAT#c.yrssince00 ib1.RACECAT#c.yrssince00, family (binomial 1) link (logit) corr(ar1)
 
*We look at the working correlation matrix
estat wcorrelation
 
*Results from Wald Tests
test 1.MARCAT 3.MARCAT
test 2.EDCAT 3.EDCAT 4.EDCAT
test 2.RACECAT 3.RACECAT 4.RACECAT
test c.yrssince00#1.MARCAT c.yrssince00#3.MARCAT 
test c.yrssince00#2.EDCAT c.yrssince00#3.EDCAT c.yrssince00#4.EDCAT
test c.yrssince00#2.RACECAT c.yrssince00#3.RACECAT c.yrssince00#4.RACECAT
 
 
* We now need to compare the QICs. This can be done by using a third party QIC Stata package
ssc install qic

* This package is old and requires all interactions and factor variable values to be classified into their own variables before they can be put into the syntax
gen MARCAT1=1.MARCAT
gen MARCAT3=3.MARCAT
gen EDCAT2=2.EDCAT
gen EDCAT3=3.EDCAT
gen EDCAT4=4.EDCAT
gen RACECAT2=2.RACECAT
gen RACECAT3=3.RACECAT
gen RACECAT4=4.RACECAT
gen M1Y= 1.MARCAT#c.yrssince00
gen M3Y=3.MARCAT# c.yrssince00
gen E2Y=2.EDCAT# c.yrssince00
gen E3Y=3.EDCAT# c.yrssince00
gen E4Y=4.EDCAT# c.yrssince00
gen R2Y=2.RACECAT# c.yrssince000
gen R3Y=3.RACECAT# c.yrssince00
gen R4Y=4.RACECAT# c.yrssince00

* First the QIC for the unstructured model
qic top25 MARCAT1 MARCAT3 EDCAT2 EDCAT3 EDCAT4 RACECAT2 RACECAT3 RACECAT4 yrssince00 M1Y M3Y R2Y R3Y R4Y E2Y E3Y E4Y,  i(HHIDPN) t(yrssince00) family (binomial 1) link (logit) corr(unstructured)

* Then the QIC for the AR1 model
qic top25 MARCAT1 MARCAT3 EDCAT2 EDCAT3 EDCAT4 RACECAT2 RACECAT3 RACECAT4 yrssince00 M1Y M3Y R2Y R3Y R4Y E2Y E3Y E4Y,  i(HHIDPN) t(yrssince00) family (binomial 1) link (logit) corr(ar1)

