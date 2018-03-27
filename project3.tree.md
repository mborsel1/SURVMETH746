SAS code: data management and recoding
libname gss 'F:\DATA\GSS';
libname cart 'F:\mborsel\ANALYSIS\PSM\SurvMeth746\CART';
data work.gss;
set gss.GSS7216_R2;
/* estimate date of birth */
array bmonth{12} _temporary_ (4 5 6 7 8 9 10 11 12 1 2 3);
array bday{12} _temporary_ (19 20 20 22 22 22 22 21 21 19 18 20);
if cohort ge 1883 and cohort le 1999 then do;
do lcv=1 to 12;
if lcv eq zodiac then dob = mdy(bmonth{lcv},bday{lcv},cohort);
end;
if dob eq . then dob = mdy(12,31,cohort);
end;
eday2012 = mdy(11,6,2012);
eday2008 = mdy(11,4,2008);
eday2004 = mdy(11,2,2004);
eday2000 = mdy(11,7,2000);
if dob gt .z then do;
if yrdif(dob,eday2012) ge 18 then ageElig2012 = 1; else ageElig2012=0;
if yrdif(dob,eday2008) ge 18 then ageElig2008 = 1; else ageElig2008=0;
if yrdif(dob,eday2004) ge 18 then ageElig2004 = 1; else ageElig2004=0;
if yrdif(dob,eday2000) ge 18 then ageElig2000 = 1; else ageElig2000=0;
end;
/* collapse "other" religions */
if relig in (6 7 8 9 10 11 12 13) then relig = 5;
/*race ethnicity */
if hispanic ge 2 and hispanic le 50 then raceeth = 3;
else if hispanic eq 1 and race eq 1 then raceeth = 1;
else if hispanic eq 1 and race eq 2 then raceeth = 2;
else if hispanic eq 1 and race eq 3 then raceeth = 4;
/* dwelling: duplex and apartment */
if dwelling in (3 4) then dwelling = 3;
else if dwelling in (7 8 9) then dwelling = 7;
/* R's that self-report as ineligible to vote are set missing */
if vote12 eq 1 then voted12 = 1;
else if vote12 eq 2 then voted12 = 0;
if vote08 eq 1 then voted08 = 1;
12else if vote08 eq 2 then voted08 = 0;
if vote04 eq 1 then voted04 = 1;
else if vote04 eq 2 then voted04 = 0;
if vote00 eq 1 then voted00 = 1;
else if vote00 eq 2 then voted00 = 0;
if partyid eq 3 then partisan = 0;
else if partyid in (2 4) then partisan = 1;
else if partyid in (1 5) then partisan = 2;
else if partyid in (0 6) then partisan = 3;
if polviews eq 4 then intensity = 0;
else if polviews in (3 5) then intensity = 1;
else if polviews in (2 6) then intensity = 2;
else if polviews in (1 7) then intensity = 3;
data cart.gss2014 (keep=id year voteprev votedprev vote voted partyid polviews partisan intensity
marital relig region raceeth sex class age dwelling wrkstat degree income06 attend coninc
ageElig ageEligPrev WTSSALL);
set work.gss;
if year eq 2014 and voted12 in (0 1) and ageElig2012 eq 1 and voted08 in (0 1) and
ageElig2008 eq 1;
voteprev = vote08;
votedprev = voted08;
ageEligPrev = ageElig2008;
vote = vote12;
voted = voted12;
ageElig = ageElig2012;
data cart.gss2010 (keep=id year voteprev votedprev vote voted partyid polviews partisan intensity
marital relig region raceeth sex class age dwelling wrkstat degree income06 attend coninc
ageElig ageEligPrev WTSSALL);
set work.gss;
if year eq 2010 and voted08 in (0 1) and ageElig2008 eq 1 and voted04 in (0 1) and
ageElig2004 eq 1;
voteprev = vote04;
votedprev = voted04;
ageEligPrev = ageElig2004;
vote = vote08;
voted = voted08;
ageElig = ageElig2008;
data cart.gss2006 (keep=id year voteprev votedprev vote voted partyid polviews partisan intensity
marital relig region raceeth sex class age dwelling wrkstat degree income06 attend coninc ageElig
ageEligPrev WTSSALL);
set work.gss;
if year eq 2006 and voted04 in (0 1) and ageElig2004 eq 1 and voted00 in (0 1) and ageElig2000 eq 1;
13run;
voteprev = vote00;
votedprev = voted00;
ageEligPrev = ageElig2000;
vote = vote04;
voted = voted04;
ageElig = ageElig2004;
proc freq data=cart.gss2014;
tables voted votedprev marital relig region raceeth sex class dwelling wrkstat degree partisan intensity;
run;
proc univariate data=cart.gss2014;
var coninc attend partisan intensity age wtssall;
run;
R code: Classification tree
sink("F:/mborsel/ANALYSIS/PSM/SurvMeth746/CART/gssVoteOutput.txt", append=FALSE,
split=TRUE)
library(rpart)
library(rpart.plot)
library(descr)
# build tree using 2014 GSS
load("F:/mborsel/ANALYSIS/PSM/SurvMeth746/CART/gss2014.Rdata")
# outcome variable is voted in 2012 election
# relax complexity parameter constraint to look for all useful splits
g.control <- rpart.control(minsplit=10, cp=0.001)
tree1 <- rpart(voted ~ factor(votedprev) + factor(marital) + factor(relig) + factor(region)
+ factor(raceeth)
+ factor(sex) + factor(class) + factor(dwelling) + factor(wrkstat) + factor(degree) + coninc
+ attend + partisan + intensity + age + wtssall, data = gss2014, method =
"class",control=g.control)
printcp(tree1)
plotcp(tree1)
g.control <- rpart.control(minsplit=10, cp=0.048)
tree2 <- rpart(voted ~ factor(votedprev) + factor(marital) + factor(relig) + factor(region) + factor(raceeth)
+ factor(sex) + factor(class) + factor(dwelling) + factor(wrkstat) + factor(degree) + coninc
+ attend + partisan + intensity + age + wtssall, data = gss2014, method = "class")
printcp(tree2)
plotcp(tree2)
summary(tree2)
rpart.plot(tree2)
#treat sample weight as WEIGHT rather than using as a predictor
tree3 <- rpart(voted ~ factor(votedprev) + factor(marital) + factor(relig) + factor(region) + factor(raceeth)
14+ factor(sex) + factor(class) + factor(dwelling) + factor(wrkstat) + factor(degree) + coninc
+ attend + partisan + intensity + age, data = gss2014, weights = wtssall, method = "class")
printcp(tree3)
plotcp(tree3)
summary(tree3)
rpart.plot(tree3)
#training data confusion matrix
tree.pred2014<-predict(tree1,gss2014,type="class")
CrossTable(gss2014$voted,tree.pred2014,prop.chisq = FALSE)
#external validation of tree using 2010 and 2006 GSS
load("F:/mborsel/ANALYSIS/PSM/SurvMeth746/CART/gss2010.Rdata")
tree.pred2010<-predict(tree1,gss2010,type="class")
CrossTable(gss2010$voted,tree.pred2010,prop.chisq = FALSE)
load("F:/mborsel/ANALYSIS/PSM/SurvMeth746/CART/gss2006.Rdata")
tree.pred2006<-predict(tree1,gss2006,type="class")
CrossTable(gss2006$voted,tree.pred2006,prop.chisq = FALSE)
