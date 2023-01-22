#The file contains R script for answers to question 3 of
#assignment 4. This is submitted for course work of Statistical
#Structures in Data, PGDBA 08, 2022-23 by Anurag Shukla (22BM6JP08)

#Importing Libraries
library("readxl")

#Importing the Dataset
setwd("C:/Users/Anurag Shukla/Documents/pgdba/study/ISI/SSD/assignment2")
df <- read_excel("Assignment_2_data.xlsx", sheet = 5)

#Multivariate Linear Regression
dof <- 34 #n-k-1
fit <- lm(SO2~temp+manu+popul+wind+precip+predays,data = df)
summary(fit)
par(mfrow = c(2,2))
plot(fit)
conf_int <- confint(fit)
write.csv(conf_int, file = "conf_int.csv")

#Testing on given X value
beta <- fit$coefficients
y_actual <- 20
x <- data.frame(int = 1, temp=55, manu=440, popul=500, wind=10.0, precip=11.75, predays=80)
y_predict <- sum(beta*x)

#Confidence and Prediction Intervals
predict(fit, x, interval="confidence")
predict(fit, x, interval="predict")

#Influencial Points using DFFITS
dffits <- as.data.frame(dffits(fit))
thres <- 2*sqrt(6/41) #2(k/n)^0.5
points <- which(abs(dffits) > thres)

#Regression without Influencial points
df1 <- df[-points,]
fit1 <- lm(SO2~temp+manu+popul+wind+precip+predays,data = df1)
summary(fit1)
par(mfrow = c(2,2))
plot(fit1)
