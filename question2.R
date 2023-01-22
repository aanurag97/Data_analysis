#The file contains R script for answers to question 2 of
#assignment 2. This is submitted for course work of Statistical
#Structures in Data, PGDBA 08, 2022-23 by Anurag Shukla (22BM6JP08)

#Importing libraries
library("readxl")
library("FactoMineR")

#Importing the dataset
setwd("C:/Users/Anurag Shukla/Documents/pgdba/study/ISI/SSD/assignment2")
df <- read_excel("Assignment_2_data.xlsx", sheet = 1)
df1 <- df[, c(2:27)]
chisq.test(df1)

#Performing Correspondence Analysis
letters <- CA(df1)
plot(letters)
write.csv(letters$eig, file = "ca_eigen.csv")
