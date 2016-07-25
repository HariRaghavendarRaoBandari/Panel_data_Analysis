###R-CODE-01-LECTURE-PANEL-DATA-ANALYSIS-IN-MARKETING
###REFRESHMENT IN REGRESSION ANALYSIS
###TO RUN CODE: MARK THE CODE AND PRESS F5 (OR PRESS F5 TO RUN THE CODE IN ONE LINE)
###R EXECUTES COMMANDS LINE BY LINE (USING # DISABLES "CODE" TO BE EXECUTED)

##################
###HEADER-START###
##################
#Load packages
#install.packages('memisc', repos='http://ftp5.gwdg.de/pub/misc/cran/')
#install.packages('car', repos='http://ftp5.gwdg.de/pub/misc/cran/')
library(memisc)
library(car)
#Load data from specific directory
#Example is for my download folder where I have saved beer.csv
#Dont forget to use "/" instead of "\"
beer<-read.csv2("C:/Users/oelshie/Downloads/beer.csv")
#Change class and levels of variable "Region" in data.frame "beer"
beer$Region<-factor(beer$Region)
levels(beer$Region)<-c("N","E","S","W")
#Compute new variables
beer$Sales.Average<-factor(ifelse(beer$Sales.Becks>mean(beer$Sales.Becks),"Yes","No"))
beer$noise<-rnorm(30,0,1)
beer$Price.Diff<-beer$Price.Becks-beer$Price.Heineken
##################
###HEADER-END#####
##################

#####SLIDE 13 Test of sample means

#Perform ANOVA for "Sales.Becks" as dependent variable and "Region" as independent variable
aov(beer$Sales.Becks~beer$Region)
#Save as object
varianz<-aov(beer$Sales.Becks~beer$Region)
#Use summary()/str()-function to aov()-object "varianz"
summary(varianz)
str(varianz)
#Extract and display coefficients/residuals of "varianz"
varianz$coefficients
varianz$residuals


#####SLIDE 15 Linear Regression (OLS)

#Perform OLS regression with "Sales.Becks" as dependent variable and "Price.Becks" as independent variable
#"data=" statement in lm() makes the "beer$" statement obsolete
lm(Sales.Becks~Price.Becks,data=beer)
#Save as object
ols1<-lm(Sales.Becks~Price.Becks,data=beer)
#Use summary()/str()-function to lm()-object "ols1"
summary(ols1)
str(ols1)
#Extract and display fitted values/coefficients/residuals of "ols1"
ols1$fitted.values
ols1$coefficients
ols1$residuals
#Extract and display fitted values/coefficients/residuals as functions to lm()-object "ols1"
fitted(ols1)
coef(ols1)
resid(ols1)


#####SLIDE 16 Logarithmic transformations

#Linear-Linear (same model as slide 15)
linlin<-lm(Sales.Becks~Price.Becks,data=beer)
#Linear-Log
linlog<-lm(Sales.Becks~log(Price.Becks),data=beer)
#Log-Linear
loglin<-lm(log(Sales.Becks)~Price.Becks,data=beer)
#Log-Log
loglog<-lm(log(Sales.Becks)~log(Price.Becks),data=beer)
#Compare results from the four different specifications
mtable(linlin,linlog,loglin,loglog)


#####SLIDE 17 Elasticity
#Compute individual level elasticity and summarize
#Linear-Linear
elast1<-coef(linlin)[2]*(beer$Price.Becks/beer$Sales.Becks)
summary(elast1)
#Linear-Log
elast2<-coef(linlog)[2]/beer$Sales.Becks
summary(elast2)
#Log-Linear
elast3<-coef(loglin)[2]*beer$Price.Becks
summary(elast3)
#Log-Log
elast4<-coef(loglog)[2]
summary(elast4)


#####SLIDE 18 Exercise (with marketing topic)

#1. Estimate (and evaluate) linear regression models using the beer data
#Linear-Linear
ols2<-lm(Sales.Becks~Price.Becks+Display.Becks+Price.Heineken,data=beer)
summary(ols2)
#2. Compute (own-)price elasticity for Becks sales (for Linear-Linear)
pe<-coef(ols2)[2]*(beer$Price.Becks/beer$Sales.Becks)
summary(pe)
#3. Compute display elasticity for Becks sales (for Linear-Linear)
de<-coef(ols2)[3]*(beer$Display.Becks/beer$Sales.Becks)
summary(de)
#4. Compute (own-)price elasticity for Becks sales (for Linear-Linear)
ce<-coef(ols2)[4]*(beer$Price.Heineken/beer$Sales.Becks)
summary(ce)
#5. Discuss how you can use these results to decide about optimal price and display levels



