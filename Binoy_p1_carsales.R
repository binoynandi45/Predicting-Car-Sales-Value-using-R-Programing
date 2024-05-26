Project:- Car Sales.
Name-Binoyananda Nandi


## Importing the libraries:-
library("boot")
library("car")
library("QuantPsyc")
library("lmtest")
library("nortest")
library("Vars")
library("sandwich")
library("MASS")

## Importing the dataset:-
setwd("D://ivy professional school//IVY notes//R//Industry Projects//Linear Regression car sales")
data<-read.csv("Car_sales.csv")

## Viewing the data set:-
head(data)
View(data)

## Checking the summary of the dataset
summary(data)
-----------------------------------------------------------------------------------
1. There are huge gap difference from 75th quantile to 100th this indicates outliers.
2. few columns have missing values denoted as NA
3. Vehicle type has 2 unique vales- passenger and car
------------------------------------------------------------------------------------

##Checking the datatypes of the dataset
str(data)
----------------------------------------------------------------------------------
1. There are 13 columns and 157 rows
2. Only on float type column
------------------------------------------------------------------------------------

## Shape of dataset:
nrow(data)


## Checking the missing values;
sapply(data,function(x)sum(is.na(x)))

which(is.na(data$Horsepower))
which(is.na(data$Wheelbase))
which(is.na(data$Engine_size))
which(is.na(data$Fuel_capacity))
which(is.na(data$Power_perf_factor))
which(is.na(data$Curb_weight))


-----------------------------------------------------------
1. 11 columns have missing values
2. the 34th index/row has most of the missing values.
-----------------------------------------------------------

##Handling Missing values:
**Deleting all the missing value rows**
data1<-na.omit(data)
nrow(data)-nrow(data1)

** Deleting the 34th row**
data1<-data[-c(34),]
nrow(data)


** Imputing missing values with mean**
data[is.na(data$Power_perf_factor),13]<-mean(data$Power_perf_factor,na.rm=TRUE)
data[is.na(data$Fuel_efficiency),12]<-mean(data$Fuel_efficiency,na.rm=TRUE)
data[is.na(data$Fuel_capacity),11]<-mean(data$Fuel_capacity,na.rm=TRUE)
data[is.na(data$Curb_weight),10]<-mean(data$Curb_weight,na.rm=TRUE)
data[is.na(data$Length),9]<-mean(data$Length,na.rm=TRUE)
data[is.na(data$Width),8]<-mean(data$Width,na.rm=TRUE)
data[is.na(data$Wheelbase),7]<-mean(data$Wheelbase,na.rm=TRUE)
data[is.na(data$Horsepower),6]<-mean(data$Horsepower,na.rm=TRUE)
data[is.na(data$Engine_size),5]<-mean(data$Engine_size,na.rm=TRUE)
data[is.na(data$Price_in_thousands),4]<-mean(data$Price_in_thousands,na.rm=TRUE)
data[is.na(data$Resale_value),2]<-mean(data$Resale_value,na.rm=TRUE)

##Rechecking the missing values:-
sapply(data,function(x)sum(is.na(x)))

data<-na.omit(data)
sapply(data,function(x)sum(is.na(x)))

### Checking the outliers:-

boxplot(data)
-----------------------------------------------------------------------------------------------------
1. maximum outlier is in salesinthousand
2. few outlier in horsepower
3. fuel efficiency also has outliers.
------------------------------------------------------------------------------------------------------

#boxplot(data$Sales_in_thousands)
#quantile(data$Sales_in_thousands,c(0,0.5,0.6,0.7,0.8,0.9,1))
#quantile(data$Sales_in_thousands,c(0,0.8,0.9,0.95,0.96,0.97,0.98,0.99,0.995,1))

#data<-data[data$Sales_in_thousands<335,]
#boxplot(data$Sales_in_thousands)

#boxplot(data$Horsepower)
#quantile(data$Sales_in_thousands,c(0,0.8,0.9,0.95,0.96,0.97,0.98,0.99,0.995,1))
#data<-data[data$Horsepower<250,]
#boxplot(data$Horsepower)

#boxplot(data$Fuel_efficiency)
#quantile(data$Fuel_efficiency,c(0,0.8,0.9,0.95,0.96,0.97,0.98,0.99,0.995,1))
#data<-data[data$Fuel_efficiency<40,]
#boxplot(data$Fuel_efficiency)

boxplot(data)
nrow(data)

### Checking for duplicate values:
data[duplicated(data),]
----------------------------------------------------------------------------------------------
**No duplicate data**
---------------------------------------------------------------------------------------------


### Spliting the data set into train and test
library("caret")
data_sp<-createDataPartition(data$Sales_in_thousands,p=0.70,list=FALSE)
head(data_sp)

train<-data[data_sp,]
head(train)
test<-data[-data_sp,]
head(test)

### Model building:-
colnames(data)
model_1<-lm(Sales_in_thousands~.,data=data)
summary(model_1)


model_2<-lm(Sales_in_thousands~.,data=train)
summary(model_2)

model_3<-lm(Sales_in_thousands~Resale_value+Vehicle_type+Price_in_thousands+Engine_size+Horsepower+Wheelbase+Width+Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Power_perf_factor,data=train)
summary(model_3)

--------------------------------------------------------------------------------------------------


## Checking the assumptions
** Multicolinearity**
colnames(data)
Data_Ml<-data[,-3]
View(Data_Ml)
cor(Data_Ml)
------------------------------------------------------------------------------------------------
1. Corelated resale value and price in thousands - dropping resale value
2. Dropping Engine size - As engine size and horse power correlated
3. Dropping length - As length and wheelbase correlated
----------------------------------------------------------------------------------------------

model_4<-lm(Sales_in_thousands~Vehicle_type+Price_in_thousands+Engine_size+Horsepower+Wheelbase+Width+Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Power_perf_factor,data=train)ency+Power_perf_factor,data=train)
summary(model_4)

vif(model_4)


model_5<-lm(Sales_in_thousands~Vehicle_type+Price_in_thousands+Horsepower+Wheelbase+Width+Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Power_perf_factor,data=train)ctor,data=train)
summary(model_5)

model_6<-lm(Sales_in_thousands~Vehicle_type+Price_in_thousands+Horsepower+Width+Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Power_perf_factor,data=train)
summary(model_6)

model_7<-lm(Sales_in_thousands~Vehicle_type+Horsepower+Width+Length+Curb_weight+Fuel_capacity+Fuel_efficiency,data=train)
summary(model_7)

model_8<-lm(Sales_in_thousands~Vehicle_type+Horsepower+Width+Length+Curb_weight+Fuel_capacity,data=train)
summary(model_8)

model_9<-lm(Sales_in_thousands~Vehicle_type+Horsepower+Width+Length+Curb_weight,data=train)
summary(model_9)

--------------------------------------------------------------------------------------------------------
** so it can be seen that though significance of variable increased with deleting non significant variables bt
** the R2 is low bt the F statistic says that there is significant change in sales for the variables.
---------------------------------------------------------------------------------------------------------

*** Normality Test *** - to check normal distribution of residuals

fitted(model_3)
resid<-model_3$residuals
shapiro.test(resid)
ad.test(resid)
--------------------------------------------------------------------------------------
#Correcting for non normality is not always necessary
#Rejecting the null hypothesis that data is normal since p value is less than 0.05
--------------------------------------------------------------------------------------

*** Checking for heteroscedasticity***
bptest(model_3)
--------------------------------------------------------------------------------------
p value is less than 0.05 ,so the assumptions is violated that residuals are equally distributed
-------------------------------------------------------------------------------------

***Checking for autocorrelation***- residuals should not be correlated
dwt(model_3)

-----------------------------------------------------------------------------------
p>0.05 means there is no auto correlation.
----------------------------------------------------------------------------------

### Predicting on test dataset

predict<-predict(model_3,test)
predict

### Checking Mape:-
predict2<-cbind(test,predict)
View(predict2)

predict2$Error<- abs(((predict2$Sales_in_thousands - predict2$predict)
                         /(predict2$Sales_in_thousands))*100)
mean(predict2$Error,na.rm = TRUE)
-------------------------------------------------------------------------------------------
**This shows the data set is not good for predictions since its very small
---------------------------------------------------------------------------------------------

attach(data)
sum(abs((predict2$Sales_in_thousands- predict2$predict)/(predict2$Sales_in_thousands)))/nrow(data)

----------------------------------------------------------------------------------

