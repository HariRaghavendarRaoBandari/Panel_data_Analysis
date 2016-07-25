#LOAD PLM PACKAGE
library(plm)
###IMPORT-DATA
tuna<-read.csv("/Users/raghu/Documents/Panel Data Analysis/Document_folder_02_Lecture_Fixed_Effects_Models/tuna.csv")


###SLIDE 21 Exercise (with marketing topic) 
###1. Estimate ?best? 2-Way FE model
###Estimate different logarithmic transformation and evaluate by R-Square to choose "best" FE model
linlin<-plm(sales~display+rtprice,data=tuna,effect="twoways",model="within")
linlog<-plm(sales~log(display+0.01)+log(rtprice),data=tuna,effect="twoways",model="within")
loglin<-plm(log(sales)~display+rtprice,data=tuna,effect="twoways",model="within")
loglog<-plm(log(sales)~log(display+0.01)+log(rtprice),data=tuna,effect="twoways",model="within")
r.squared(linlin)
r.squared(linlog)
r.squared(loglin)
r.squared(loglog)


###"Best" 2-Way FE Model (same as loglog from above)
feno<-plm(log(sales)~log(display+0.01)+log(rtprice),data=tuna,effect="twoways",model="within")
summary(feno)
###Price elasticity
coef(feno)[2]

###2. Estimate ?best? 2SLS-2-Way-FE model 
feiv<-plm(log(sales)~log(display+0.01)+log(rtprice) | log(display+0.01)+log(wsprice),
data=tuna,effect="twoways",model="within")
summary(feiv)
###Price elasticity
coef(feiv)[2]

###Evaluate WSPRICE as instrument
###Exclusion restricition
feer<-plm(log(sales)~log(display+0.01)+log(rtprice)+log(wsprice),data=tuna,effect="twoways",model="within")
summary(feer)
vif(feer)

###3. Compute difference in parameter estimates
###a) with vs. without IV
###Difference in
#Display
coef(feno)[1]-coef(feiv)[1]
#Retail price
coef(feno)[2]-coef(feiv)[2]

###Hausman test for testing significant differences in all parameter estimates in two model
###Often used as test for endogeneity (when potentially solved by instruments)!
phtest(fe1,fe2)

###Have a look an the correlation between X and e
cor(feno$residuals,tuna$rtprice)
cor(feiv$residuals,tuna$rtprice)


###3. Compute difference in parameter estimates
###b) with vs. without omitted variable display
###Omit display
feod<-plm(log(sales)~log(rtprice) | log(wsprice),data=tuna,effect="twoways",model="within")
summary(feod)

###Difference in
#Retail Price
coef(feiv)[2]-coef(feod)[1]

###Have a look an the correlation between X and e
cor(feiv$residuals,tuna$rtprice)
cor(feod$residuals,tuna$rtprice)


##COMPUTE-2SLS-BY-HAND (I.E. BOTH STAGES BY SEPARATE FE MODELS)
##CREATE PREDICTED X (FIRST STAGE)
tuna$ivprice<-log(tuna$rtprice)-plm(log(rtprice)~log(display+0.01)+log(wsprice),
data=tuna,effect="twoways",model="within")$residuals
##USE PREDICTED X IN MAIN REGRESSION MODEL (SECOND STAGE)
febh<-plm(log(sales)~log(display+0.01)+ivprice,data=tuna,effect="twoways",model="within")
summary(febh)


#8. Estimate the ?best? FE model with a HAC covariance matrix
coeftest(feiv,vcovHC(feiv,method="arellano",cluster = "group"))


