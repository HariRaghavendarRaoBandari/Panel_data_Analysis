###R-CODE-02-LECTURE-PANEL-DATA-ANALYSIS-IN-MARKETING
###FIXED-EFFECTS-MODELS
###TO RUN CODE: MARK THE CODE AND PRESS F5 (OR PRESS F5 TO RUN THE CODE IN ONE LINE)
###R EXECUTES COMMANDS LINE BY LINE (USING # DISABLES "CODE" TO BE EXECUTED)


##################
###HEADER-START###
##################
#Load packages
#install.packages('plm', repos='http://ftp5.gwdg.de/pub/misc/cran/')
#install.packages('car', repos='http://ftp5.gwdg.de/pub/misc/cran/')
#install.packages('lmtest', repos='http://ftp5.gwdg.de/pub/misc/cran/')
#install.packages('memisc', repos='http://ftp5.gwdg.de/pub/misc/cran/')
library(plm)
library(car)
library(lmtest)
library(memisc)
#Load data from specific directory
#Example is for my download folder where I have saved beer.csv
#Dont forget to use "/" instead of "\"
slice<-read.csv2("/Users/raghu/Documents/Panel Data Analysis/Document_folder_02_Lecture_Fixed_Effects_Models/slice.csv")
##################
###HEADER-END#####
##################

#Compute summary statistics for data.frame "slice"
summary(slice)

#sapply()-function applies FUNCTION to each column in data.frame DATA: sapply(DATA,FUCNTION)
#Check class for each column in data.frame "slice"
sapply(slice,class)

#Display scatterplot-matrix for variables in column 3 to 5 in data.frame "slice"
plot(slice[,c(3:5)],cex=2)

#Use boxplot to display hetergeneity in "sales" across stores and weeks
#ylim = Limit X-Axis between 0 and 45000 
#Stores
boxplot(slice$sales~slice$store,ylim=c(0,45000))
#Weeks
boxplot(slice$sales~slice$week,ylim=c(0,45000))

#####SLIDE 13 Exercise (with marketing topic)

#1. Estimate an OLS model
ols<-lm(sales~price+display,data=slice)
summary(ols)

#2. Estimate LSDV models
#One-way FE for "store" (remove Intercept by "-1" in formula)
lsdv1<-lm(sales~price+display+store-1,data=slice)
summary(lsdv1)
#One-way FE for "week" (remove Intercept by "-1" in formula and make "week" a factor))
lsdv2<-lm(sales~price+display+factor(week)-1,data=slice)
summary(lsdv2)
#Two-way FE for "stores" and "week" (and remove Intercept by "-1" in formula)
lsdv3<-lm(sales~price+display+store+factor(week)-1,data=slice)
summary(lsdv3)


#3. Estimate FE models (plm() uses demeaning procedure)
#See Help for plm()-function
?plm
#Dataset must have individuals and time-periods ordered in first two columns
#Relevant arguments
#effect = c("individual", "time", "twoways")
#model = c("within", "random", "ht", "between", "pooling", "fd")
#One-way FE for "store"
fe1<-plm(sales~price+display,data=slice,effect="individual",model="within")
summary(fe1)
#Compare price coefficients from LSDV1 and FE1 model
coef(fe1)[1]
coef(lsdv1)[1]
#One-way FE for "week"
fe2<-plm(sales~price+display,data=slice,effect="time",model="within")
summary(fe2)
#Compare price coefficients from LSDV2 and FE2 model
coef(fe2)[1]
coef(lsdv2)[1]
#Two-way FE for "stores" and "week"
fe3<-plm(sales~price+display,data=slice,effect="twoways",model="within")
summary(fe3)
#Compare price coefficients from LSDV3 and FE3 model
coef(fe3)[1]
coef(lsdv3)[1]

#4. Compare logarithmic transformations for 2-Way FE model
#Linear-Linear (same as fe3)
tw1<-plm(sales~price+display,data=slice,effect="twoways",model="within")
#Linear-Log (add +0.01 to display to avoid taking log of zero!)
tw2<-plm(sales~log(price)+log(display+0.01),data=slice,effect="twoways",model="within")
#Log-Linear
tw3<-plm(log(sales)~price+display,data=slice,effect="twoways",model="within")
#Log-Log (add +0.01 to display to avoid taking log of zero!)
tw4<-plm(log(sales)~log(price)+log(display+0.01),data=slice,effect="twoways",model="within")
#Compare models by R-square
r.squared(tw1)
r.squared(tw2)
r.squared(tw3)
r.squared(tw4)


#5. Compute price and display elasticity for ?best? FE model
#Price elasticity
#Best
coef(tw4)[1]
#Second-best
summary(coef(tw3)[1]*slice$price)

#Display elasticity
#Best
coef(tw4)[2]
#Second-best
summary(coef(tw3)[2]*slice$display)


#6. Plot ind and time for ?best? FE model
#fixef()-function extracts fixed effects from plm()-objects
#Individual fixed effects
ind<-fixef(tw4,effect="individual")
summary(ind)
#Plot hist and density of individual (store) fixed effects
hist(ind)
plot(density(ind))
#Time fixed effects
#dont use "time" as new object name because thats a function
zeit<-fixef(tw4,effect="time") 
summary(zeit)
#Plot line (type="l") of time (week) fixed effects
plot(zeit,type="l")

#7. Perform specification tests for ?best? FE model
#Cross-sectional dependence (Pesaran test)
#H0: No cross-sectional dependence (good!)
#H1: Cross-sectional dependence	(bad!)
pcdtest(tw4)

#Stationarity / No trend (Im, Pesaran and Shin test)
#H0: Y is not stationary / has a trend	(bad!)
#H1: Y is stationary / has no trend	(good!)
cipstest(pdata.frame(slice)$sales, type="trend")

#Serial auto-correlation (Breusch-Godfrey test)
#H0: No serial auto-correlation 	(good!)
#H1: Serial auto-correlation		(bad!)
pbgtest(tw4)

#Heteroskedasticity (Breusch-Pagan test)
#H0: No heteroskedasticity  	(good!)
#H1: Heteroskedasticity 	(bad!)
#Important note: Use 2-Way LSDV model
bptest(lsdv3)

#Multicollinearity (Variance Inflation Factors)
#If greater than 5, then presence of multicollinearity
vif(tw4)

#8. Estimate the ?best? FE model with a HAC covariance matrix
coeftest(tw4,vcovHC(tw4,method="arellano",cluster = "group"))