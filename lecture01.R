###R-CODE-01-LECTURE-PANEL-DATA-ANALYSIS-IN-MARKETING
###INTRODUCTION TO R
###TO RUN CODE: MARK THE CODE AND PRESS F5 (OR PRESS F5 TO RUN THE CODE IN ONE LINE)
###R EXECUTES COMMANDS LINE BY LINE (USING # DISABLES "CODE" TO BE EXECUTED)

#####SLIDE 7 Simple tasks
###Simple calculation and TRUE/FALSE calls
1+3
1/3
1==2
1==1
1>2
1<2
###Simple data generation
1:10
c(3,6,2,8,5,2)#create vector with values between ()
rep(4,16)
seq(0,10,0.5)
rnorm(100,5,10)
c(
#Saves as objects
s1<-1:10
s2<-rep(4,16)
s3<-seq(0,10,0.5)
s4<-rnorm(100,5,10)
#Help for built-in functions
?seq
?rnorm
#What objects are saved in the workspace
ls()
#What type of object
class(s1)
class(s4)

#####SLIDE 9 Data import
#What is the workingdirectory on your computer
getwd()
#Load data from specific directory
#Example is for my download folder where I have saved beer.csv
#Dont forget to use "/" instead of "\"
beer<-read.csv2("/Users/raghu/Documents/Panel Data Analysis/Document_folder_01_Lecture_Introduction_to_R_and_Refreshment_in_Regression_Analysis/beer.csv")
#Class ob beer object should be data.frame
class(beer)
#Check dimension of the data.frame "beer"
dim(beer)
#Check first few rows of the data.frame "beer"
head(beer)
#List variable names of the data.frame "beer"
names(beer)
#What elements does the object beer contain?
str(beer)
#Display object from data.frame beer by using $
beer$Sales.Becks
#Write new variables into data.frame "beer" by $
beer$noise<-rnorm(30,0,1)
beer$Price.Diff<-beer$Price.Becks-beer$Price.Heineken
head(beer$Price.Diff)
#Export data.frame beer to working directory
write.csv2(beer,"beernew.csv")
#Export data.frame beer to specific directory
write.csv2(beer,"/Users/raghu/Documents/Panel Data Analysis/Document_folder_01_Lecture_Introduction_to_R_and_Refreshment_in_Regression_Analysis/beernew.csv")


#####SLIDE 9 Descriptives and graphics
#Get summary statistics for each variable in the data.frame "beer"
summary(beer)
#Get summary statistics for second column of data.frame "beer"
summary(beer[,2])
#Indexing is used by beer[r,c] with r=row-number(s) c=column-number(s)
#For example show row 10 to 20 for column 1 to 3
beer[10:20,1:3]

#Get summary statistics for variable "Sales.Becks" from the data.frame "beer"
summary(beer$Sales.Becks)
#Get mean/median/variance/standard dev. for variable "Sales.Becks" from the data.frame "beer"
mean(beer$Sales.Becks)
median(beer$Sales.Becks)
var(beer$Sales.Becks)
sd(beer$Sales.Becks)

#Create table for variable "Region" from the data.frame "beer"
table(beer$Region)

#Plot X=Rownumber and Y=Sales.Becks from the data.frame "beer"
plot(beer$Sales.Becks)
#Plot histogram/boxplot/density of the variable "Sales.Becks" from the data.frame "beer"
hist(beer$Sales.Becks)
plot(density(beer$Sales.Becks))
boxplot(beer$Sales.Becks)


#####SLIDE 12 Data frame functions and data management
#Check object type for variable "Region" from the data.frame "beer"
class(beer$Region)
#Change object type for variable "Region" from the data.frame "beer" into factor (=categorical variable)
beer$Region<-factor(beer$Region)
#Check again object type for variable "Region" from the data.frame "beer"
class(beer$Region)
#Display levels of factor "Region" from the data.frame "beer"
levels(beer$Region)
#Change levels of factor "Region" from the data.frame "beer" (see Slide 8 Region:...)
levels(beer$Region)<-c("N","E","S","W")
#Create new variable with "Yes" for Sales above average and "No" for below
beer$Sales.Average<-factor(ifelse(beer$Sales.Becks>mean(beer$Sales.Becks),"Yes","No"))

#####SLIDE 13 Bivariate statistics
#Scatterplot for X=Price.Becks and Y=Sales.Becks from data.frame "beer"
#with larger dots cex=2
#main label "Scatterplot"
# Axis labels "Sales" and "Price"
plot(beer$Price.Becks,beer$Sales.Becks,
cex=2,
main="Scatterplot",
ylab="Sales",xlab="Price")

#Compute Covariance between "Price.Becks" and "Sales.Becks" from data.frame "beer"
cov(beer$Price.Becks,beer$Sales.Becks)
#Compute Correlation between "Price.Becks" and "Sales.Becks" from data.frame "beer"
cor(beer$Price.Becks,beer$Sales.Becks)

#Compute Correlation between "Price.Becks" and "Sales.Becks" from data.frame "beer"
#Including Test statistics and significance test for ZERO
cor.test(beer$Price.Becks,beer$Sales.Becks)

#Create 2-way frequency table for "Display.Becks" and "Region" from data.frame "beer"
table(beer$Display.Becks,beer$Region)
#Save as object tbl
tbl<-table(beer$Display.Becks,beer$Region)
#Perfomr Chi-Square-Test of Independence for "Display.Becks" and "Region" from data.frame "beer"
chisq.test(tbl)

#Perform two-sample T-Test for "Sales.Becks" as dependent variable and "Display.Becks" as independent variable
t.test(beer$Sales.Becks~beer$Display.Becks)
#See Help for t.test function
?t.test
##Arguments:
#t.test(x, y = NULL,
#       alternative = c("two.sided", "less", "greater"),
#       mu = 0, paired = FALSE, var.equal = FALSE,
#       conf.level = 0.95, ...)




