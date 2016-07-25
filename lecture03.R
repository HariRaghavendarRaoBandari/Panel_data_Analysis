###R-CODE-03-LECTURE-PANEL-DATA-ANALYSIS-IN-MARKETING
###RANDOM-EFFECTS-MODELS
###TO RUN CODE: MARK THE CODE AND PRESS F5 (OR PRESS F5 TO RUN THE CODE IN ONE LINE)
###R EXECUTES COMMANDS LINE BY LINE (USING # DISABLES "CODE" TO BE EXECUTED)

##################
###HEADER-START###
##################
#Load packages
library(plm)
library(lmtest)
#Load data from specific directory
#Example is for my download folder where I have saved orange.csv
#Dont forget to use "/" instead of "\"
juice<-read.csv2("/Users/raghu/Documents/Panel Data Analysis/Document_folder_03_Lecture_Random_Effects_Models/orange.csv")
##################
###HEADER-END#####
##################

#Compute summary statistics for data.frame "juice"
summary(juice)

###1.Compare OLS , FE and RE model
#Save formula as objects 
#"e1" (without store demographics) 
e1<-log(sales)~log(price)+log(display+0.01)
#"e2" (with store demographics)
e2<-log(sales)~log(price)+log(display+0.01)+educ+income

#Estimate OLS model (use summary() and save as object "ols" simultaneously)
summary(ols1<-lm(e1,data=juice))
summary(ols2<-lm(e2,data=juice))

#Estimate FE model (use summary() and save as object "fe" simultaneously)
summary(fe1<-plm(e1,data=juice,effect="two",model="within"))
summary(fe2<-plm(e2,data=juice,effect="two",model="within"))

#Estimate RE model (use summary() and save as object "re" simultaneously)
summary(re1<- plm(e1,data=juice,effect="two",model="random"))
summary(re2<- plm(e2,data=juice,effect="two",model="random"))


###2. Test for Ho: FE = RE
##Hausmann test
phtest(fe1,re1)
phtest(fe2,re2)


###3. Compare price and display elasticities of FE and RE model
###
##Price and Display elasticities for FE model
coef(fe1)
coef(fe2)
##Price and Display elasticities for RE model
coef(re1)[2:3]
coef(re2)[2:3]


###4. Compute HAC covariance matrix for FE and RE model
###
##for FE model
coeftest(fe1,vcovHC(fe1,method="arellano",cluster = "group"))
coeftest(fe2,vcovHC(fe2,method="arellano",cluster = "group"))
##for RE model
coeftest(re1,vcovHC(re1,method="arellano",cluster = "group"))
coeftest(re2,vcovHC(re2,method="arellano",cluster = "group"))







