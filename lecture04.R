###R-CODE-04-LECTURE-PANEL-DATA-ANALYSIS-IN-MARKETING
###DYNAMIC-PANEL-MODELS
###TO RUN CODE: MARK THE CODE AND PRESS F5 (OR PRESS F5 TO RUN THE CODE IN ONE LINE)
###R EXECUTES COMMANDS LINE BY LINE (USING # DISABLES "CODE" TO BE EXECUTED)

##################
###HEADER-START###
##################
#Load packages
library(plm)
###Function to compute R2 for pgmm()-objects
gmmR2<-function(x){
yact<-list()
for(i in 1:140){yact[[i]]<-x$model[[i]][,1]}
yhat <- matrix(x$fitted.values,ncol=1)
return(data.frame(GMM_R2=cor(unlist(yact),yhat)^2))}
#Load data from specific directory
#Example is for my download folder where I have saved orange.csv
#Dont forget to use "/" instead of "\"
work<-read.csv2("/Users/raghu/Documents/Panel Data Analysis/Document_folder_04_Lecture_Dynamic_Panel_Models/employment.csv")
##################
###HEADER-END#####
##################

#Compute summary statistics for data.frame "work"
summary(work)
#Time-periods
table(work$year)
#Heterogeneity across firms
boxplot(work$emp~work$firm)
#Heterogeneity across Years
boxplot(work$emp~work$year)


####1. Estimate-Arellano and Bond (1991) equation 
###Using OLS model
ols<-plm(log(emp)~lag(log(emp), 1:2)+lag(log(wage),0:1)+log(capital)+lag(log(output),0:1),
data=work,effect=NULL,model="pooling")
summary(ols)

###Using 2-Way FE model
fe<-plm(log(emp)~lag(log(emp), 1:2)+lag(log(wage),0:1)+log(capital)+lag(log(output),0:1),
data=work,effect="two",model="within")
summary(fe)

###Using 2-Way 2-Step GMM dynamic panel model
ab<-pgmm(log(emp)~lag(log(emp),1:2)+lag(log(wage),0:1)+log(capital)+lag(log(output),0:1) | lag(log(emp), 2:9),
            data = work, effect = "twoways", model = "twostep")
summary(ab)


####2. Compare the parameter estimates (i.e. elasticities)
##Save OLS coefficients without Intercept and with zero for time-dummies
olscc<-c(ols$coefficients[-1],rep(0,6))
##Save FE coefficients and time-dummies
fecc<-c(fe$coefficients,fixef(fe,effect="time",type = "dfirst"))
##Save GMM coefficients One-Step
ab1c<-ab$coefficients[[1]]
##Save GMM coefficients Two-Step
ab2c<-ab$coefficients[[2]]
###Display coefficients
data.frame(OLS=olscc,FE=fecc,GMM1=ab1c,GMM2=ab2c)


####3. Check the model diagnostics for the GMM model
summary(ab,time.dummies=TRUE)

#Check the elements of the pgmm()-object
#Residuals
ab$residuals 	
#Datamatrix
ab$model 	
#Matrix of GMM instruments
ab$W 	
#Weighting Matrix One-Step
ab$A1 	
#Weighting Matrix Two-Step
dim(ab$A2)


####4. Compare the R2  for the OLS, FE and GMM model
###Functions to compute R2
r.squared(ols)
r.squared(fe)
gmmR2(ab)

	
 
