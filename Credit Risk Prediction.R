### Setting up the default library ####
setwd("C:/greatlakes/fra")
getwd()
### Installing the necessary libraries ####
install.packages("readxl")
install.packages("readr")
install.packages("lattice")
install.packages("rpivotTable")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrgram")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("psych")
install.packages("car")
install.packages("mice")
install.packages("DMwR")
install.packages("caret")
install.packages("StatMeasures")
install.packages("ineq")
install.packages("ROCR")
install.packages("InformationValue")
### Invoking the necessary libraries ####
library(readxl)
library(readr)
library(lattice)
library(rpivotTable)
library(ggplot2)
library(dplyr)
library(corrgram)
library(corrplot)
library(Hmisc)
library(psych)
library(car)
library(mice)
library(DMwR)
library(caret)
library(StatMeasures)
library(ineq)
library(ROCR)
library(InformationValue)
### EDA #####
training = read_xlsx("training.xlsx")
View(training)
### Initial Exploration ####
class(training)
training = as.data.frame(training)
head(training)
tail(training)
summary(training)
colnames(training)
dim(training)
### Creation of the response variable ####
training$default = ifelse(training$`Networth Next Year`>0,"1","0")
training$default = as.factor(training$default)
table(training$default)
#### Clearing the Inconsistencies ####
### Removal of Variable with all NAs ####
training = training[,-c(22)]
### changing to numeric ####
training$`Creditors turnover` = as.numeric(training$`Creditors turnover`)
training$`Finished goods turnover` = as.numeric(training$`Finished goods turnover`)
training$`WIP turnover` = as.numeric(training$`WIP turnover`)
training$`Shares outstanding` = as.numeric(training$`Shares outstanding`)
training$`Equity face value` = as.numeric(training$`Equity face value`)
training$`PE on BSE` = as.numeric(training$`PE on BSE`)
training$`Debtors turnover` = as.numeric(training$`Debtors turnover`)
training$`Raw material turnover` = as.numeric(training$`Raw material turnover`)
###treating NAs ####
sum(is.na(training))
training2 = training
for(i in c(1:51)){
  if(sum(is.na(training2[,i])) > 0){
    training2[,i][is.na(training2[,i])] = median(training2[,i],na.rm = TRUE)
  }
}
sum(is.na(training2))

### Treating Outliers ####
training2 = as.data.frame(training2)
boxplot(training2)
outlier2 = function(i)
{
  IQ = IQR(training2[,i])
  z = quantile(training2[,i])
  z= as.data.frame(z)
  q1 = z[2,1]
  q3 = z[4,1]
  subset1 = training2[training2[,i] < q1 - 1.5*IQ,]
  lo = nrow(subset1)
  subset2 = training2[training2[,i] > q3 + 1.5*IQ,]
  hi = nrow(subset2)
  Outliers = lo + hi
  Outliers = as.numeric(Outliers)
  Outliers
}

outcol = function(j){
  k = colnames(training2[j])
  k
}

for (i in c(1:51)){ 
  if (outlier2(i) > 0){
    print(outcol(i))
    print(i)
    print(outlier2(i))
  }
}

for(g in c(1:51)){
  iq = IQR(training2[,g])
  z = quantile(training2[,g])
  z= as.data.frame(z)
  range1 = z[2,1] - 1.5*iq
  range2 = z[4,1] + 1.5*iq
  training2[,g][training2[,g]<range1] = range1
  training2[,g][training2[,g]>range2] = range2
  print(outlier2(g))
}
boxplot(training2)

###Univariate Analysis and Bi-Variate Analysis ####
## Univariate Analysis ####
hist(training2$Sales,labels = TRUE,col = "Blue",xlab = "Sales",main = "Sales")
hist(training2$`Total capital`,labels = TRUE,col = "Red",xlab = "Total Capital",main = "Total Capital")
hist(training2$`Total liabilities`,labels = TRUE,col = "Yellow",xlab = "Total Liabilities",main = "Total Liabilities")
## Bi-Variate Analysis ####
def = training2$default
levels(def)
levels(def) = c("Non-Default","Default")
qplot(training2$`Total liabilities`,fill = def,xlab = "Total Liabilities",
      main = "Liabilities vs. Defaulters")
qplot(training2$`Current ratio (times)`,fill = def,xlab = "Current Ratio",main = "Current Ratio vs. Defaulters")
qplot(training2$`Net fixed assets`,training2$Sales,fill = def,geom = "boxplot",xlab = "Fixed Assets",
      ylab = "Sales",main = "Fixed Assets vs. Sales")
### Finding Multicollinearity ####
## Correlation Plot ####
corr = training2[,-c(52)]
corr = corr[,-c(51,47)]
cor.plot(corr,numbers = TRUE)
cor.m = cor(corr)
cor.m = as.data.frame(cor.m)
value = cor.m[cor.m[]>0.90]
length(value)
## Eigen Values ####
eigen = eigen(cor(corr))
sum(eigen$values[]<0.001)

### Model Building ####
dim(training2)
training3 = training2[,-c(51,50,48,47)]

### First Iteration ####
fit = glm(default~.-`Networth Next Year`,data = training3,family = "binomial")
summary(fit)
### Second Iteration
fit2 = glm(default~Num+`PAT as % of net worth`+
             `TOL/TNW`+`Total term liabilities / tangible net worth`+`Total capital`+
             `Other income`+
             `Cumulative retained profits`+
             `TOL/TNW`+
             `Current ratio (times)`+`Debt to equity ratio (times)`+`Cash to current liabilities (times)`+
             `Adjusted EPS`,data = training3,family = "binomial")
summary(fit2)

### Introduction of new variables for Ratio Analysis ####
training4 = training3
## Ratio for Size ####
training4$Fixed.by.total = training4$`Net fixed assets`/training4$`Total assets`*100
## Profitability Ratios ####
training4$returnonassets = training4$PBT/training4$`Total assets`*100
## Leverage Ratios ####
training4$asset.equity = training2$`Total assets`/(training2$`Total assets`+training2$`Shareholders funds`)
### Liquidity Ratio ####
training4$networkingcapitalratio = (training2$`Current liabilities & provisions` - training2$`Current assets`)/(training2$`Current liabilities & provisions` - training2$`Current assets`)+training2$`Total assets`

### Final Model ####
fit3 = glm(default~`PAT as % of net worth`+`Cumulative retained profits`+`Current ratio (times)`+
             `Debt to equity ratio (times)`+`Cash to current liabilities (times)`+
             `Adjusted EPS`+Fixed.by.total+returnonassets+asset.equity
           +networkingcapitalratio,
           data = training4,family = "binomial")
summary(fit3)

### Importing the testing data ####
testing = read_excel("test.xlsx")
sum(is.na(testing))
summary(testing)
testing = as.data.frame(testing)
testing$`Default - 1` = as.factor(testing$`Default - 1`)
### EDA for the testing data ####
str(testing)
dim(testing)
f = class(testing[,52])
colnames(testing)
testing = testing[,-c(22)]
### character to numericals ####
testing$`Creditors turnover` = as.numeric(testing$`Creditors turnover`)
testing$`Debtors turnover` = as.numeric(testing$`Debtors turnover`)
testing$`Finished goods turnover` = as.numeric(testing$`Finished goods turnover`)
testing$`WIP turnover` = as.numeric(testing$`WIP turnover`)
testing$`Raw material turnover` = as.numeric(testing$`Raw material turnover`)
testing$`Shares outstanding` = as.numeric(testing$`Shares outstanding`)
testing$`Equity face value` = as.numeric(testing$`Equity face value`)
testing$`PE on BSE` = as.numeric(testing$`PE on BSE`)
str(testing)

summary(testing)

### Treating NAs for testing data ####
for(i in c(1:51)){
  if(sum(is.na(testing[,i])) > 0){
    testing[,i][is.na(testing[,i])] = median(testing[,i],na.rm = TRUE)
  }
}
sum(is.na(testing))
#### Treating outliers for testing data ####
outlier23 = function(i)
{
  IQ = IQR(testing[,i])
  z = quantile(testing[,i])
  z= as.data.frame(z)
  q1 = z[2,1]
  q3 = z[4,1]
  subset1 = testing[testing[,i] < q1 - 1.5*IQ,]
  lo = nrow(subset1)
  subset2 = testing[testing[,i] > q3 + 1.5*IQ,]
  hi = nrow(subset2)
  Outliers = lo + hi
  Outliers = as.numeric(Outliers)
  Outliers
}

outcol2 = function(j){
  k = colnames(testing[j])
  k
}
colnames(testing)
str(testing)
for (i in c(3:51)){ 
  if (outlier23(i) > 0){
    print(outcol2(i))
    print(i)
    print(outlier2(i))
  }
}

for(g in c(3:51)){
  iq = IQR(testing[,g])
  z = quantile(testing[,g])
  z= as.data.frame(z)
  range1 = z[2,1] - 1.5*iq
  range2 = z[4,1] + 1.5*iq
  testing[,g][testing[,g]<range1] = range1
  testing[,g][testing[,g]>range2] = range2
  print(outlier2(g))
}
boxplot(testing)

## Ratio for Size ####
testing$Fixed.by.total = testing$`Net fixed assets`/testing$`Total assets`*100
## Profitability Ratios ####
testing$returnonassets = testing$PBT/testing$`Total assets`*100
## Leverage Ratios ####
testing$asset.equity = testing$`Total assets`/(testing$`Total assets`+testing$`Shareholders funds`)
### Liquidity Ratio ####
testing$networkingcapitalratio = (testing$`Current liabilities & provisions` - testing$`Current assets`)/(testing$`Current liabilities & provisions` - testing$`Current assets`)+testing$`Total assets`

### Prediction and Comparision ####
validprediciton0 = predict(fit3,newdata = testing,type = "response")
validprediciton0
plot(testing$`Default - 1`,validprediciton0)
abline(a=0.75,b = 0,col = "Red")                        
validprediciton = ifelse(validprediciton0<0.75,"1","0")

### Confusion Matrix ####
validprediciton = as.factor(validprediciton)
validprediciton = as.factor(validprediciton)
caret::confusionMatrix(testing$`Default - 1`,validprediciton,positive = "1")
### Concordance Ratio ####
validprediciton.prob1 = 1 - validprediciton0
x= testing$`Default - 1`
y = validprediciton.prob1
Concordance(actuals = x,predictedScores = y)
## PLOTTING THE TPR AND FPR GRAPH AND FINDING ROC ###
pred.obj = prediction(validprediciton.prob1,valid$`Default - 1`)
perf = performance(pred.obj,"tpr","fpr")
plot(perf,main = "ROC")
#### AUC ####
auc = performance(pred.obj,"auc")
auc = as.numeric(auc@y.values)
print(auc)
### KS VALUE ###
print(max(perf@y.values[[1]] - perf@x.values[[1]]))
#### GINI COEFFICIENT ####
gini1 = ineq(validprediciton.prob1,"gini")
print(gini1)

### Creation of Deciles ####
validprediciton.prob1 = 1 - validprediciton0
testing$probabilityofdefault = round(validprediciton.prob1,digits = 3)
testing$predictedvalue = validprediciton
testing1 = testing
testing1$decile = cut(testing1$probabilityofdefault,seq(0,1,0.1),labels = c(1:10))
testing1$decile[is.na(testing1$decile)] = 1
testing1$decile
testing1 = testing1[order(desc(testing1$probabilityofdefault)),]
View(testing1)
write_csv(testing1,"C:/greatlakes/fra/Validation data (deciled).csv")

