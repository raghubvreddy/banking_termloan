#Bank Marketing data prediction using KNN Classifier
#Dataset consists of 41K+ records related to Marketing campaign of a Portugese bank and used to predict if 
#a customer will subscribe to a Term Deposit or not
library(caTools)
library(gmodels)
library(DMwR)
library(class)
#Reading Bank Marketing Campaing Data
Bankdata=read.csv("bank-additional-full.csv")
#View(Bankdata)
#str(Bankdata) # consists of 41188 obs. with 21 variables
summary(Bankdata)
##-----EDA-----##
#Univariate Analysis
summary(Bankdata$AGE)
boxplot(Bankdata$AGE, horizontal = TRUE)
x1=boxplot.stats(Bankdata$AGE)
str(x1) #Age variable has min value as 17 and max as 98 and num# of outliers are 469
summary(Bankdata$DURATION)
x2=boxplot.stats(Bankdata$DURATION) #DURATION variable has min value as 0 and max as 4918 Secs and num# of outliers are 2963
str(x2)
summary(Bankdata$CAMPAIGN)
x3=boxplot.stats(Bankdata$CAMPAIGN)
str(x3) #CAMPAIGN variable has min value as 1 and max as 56  and num# of outliers are 2406 and 56 is an extreme outlier and need to be removed during analysis
summary(Bankdata$PDAYS) #999 means client was not previously contacted and for this campaign, is a new client
str(Bankdata$PDAYS)
boxplot.stats(Bankdata$PDAYS)
x4=as.integer(Bankdata$PDAYS)
str(x4)
count=0
y=0
predays=Bankdata$PDAYS
for(i in 1:41188){
  if(predays[i]==999){
    count=count+1
  }
  else{
    y=y+1
  }
}
print(count) #39673
print(y) #1515
##There are 1515 customers who have been contacted as part of previous campaign and rest are new target customers
##This data needs to be converted to 1 new column to segregate new and old customers
summary(Bankdata$PREVIOUS)
x5=boxplot.stats(Bankdata$PREVIOUS)
str(x5)
boxplot(Bankdata$PREVIOUS, horizontal = TRUE)
hist(Bankdata$PREVIOUS)
#sum(Bankdata$PREVIOUS != 7)
table(Bankdata$PREVIOUS) #There is only 1 customer with value previous=7 and majority are not contacted
table(Bankdata$POUTCOME)
#Bank had contacted total 5625 customers in the previous campaign and converted  1373 of them to success and 
#the conversion rate is 24.4%
table(Bankdata$JOB)
#aggregate(Bankdata$Job, Bankdata$y == "yes", length)
table(Bankdata$MARITAL)
table(Bankdata$EDUCATION)
table(Bankdata$DEFAULT)
table(Bankdata$HOUSING)
table(Bankdata$LOAN)
table(Bankdata$CONTACT)
table(Bankdata$MONTH)
table(Bankdata$DAY_OF_WEEK)
plot(Bankdata$DAY_OF_WEEK)
boxplot(Bankdata$EMP.VAR.RATE)
table(Bankdata$CONS.PRICE.IDX)
table(Bankdata$CONS.CONF.IDX)
table(Bankdata$EURIBOR3M)
table(Bankdata$NR.EMPLOYED)
#Checking on the number of "Unknown"s in the given dataset
sum(Bankdata == "unknown") #12718
summary(Bankdata$JOB)
summary(Bankdata$EDUCATION)
summary(Bankdata$DEFAULT) # 8597
################################
#converting DEFAULT unknowns to Yes as there will be only 2 options for a Default: No and Yes
# str(Bankdata$DEFAULT)  
# def1=Bankdata$DEFAULT
# str(def1)
# def1[7441]
# Bankdata=cbind(bankd)
# count=0
# for(i in 1:41188)
# {
#   if(def1[i]=="unknown"){
#     def1[i]="yes"    
#     count=count+1
#   }
#   }
# print(count)
# View(def1)
# summary(def1)

#kNN imputation for Unknown values after converting them to NA's
BankdataUpd <- lapply(Bankdata, gsub, pattern = "unknown", replacement = NA, fixed = TRUE)
BankdataUpd=as.data.frame(BankdataUpd)
BankdataUpd=knnImputation(BankdataUpd,k=10)