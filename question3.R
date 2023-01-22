#The file contains R script for answers to question 3 of
#assignment 2. This is submitted for course work of Statistical
#Structures in Data, PGDBA 08, 2022-23 by Anurag Shukla (22BM6JP08)

#Importing the dataset
setwd("C:/Users/Anurag Shukla/Documents/pgdba/study/ISI/SSD/assignment2")
df <- read.csv("winequality-red.csv")
df1 <- df[,-12]

#Feasible number of Factors
p <- 11
s <- rep(0,p)
for (k in 1:11) s[k]=0.5*((p-k)^2-(p+k))

#Optimum number of factors
corr <- cor(df1)
eig <- eigen(corr)$values
plot(eig)
title("Scree Plot of Eigen Values")

#Final Model with and without Rotation
fac <- factanal(df1, 4, scores="Bartlett", rotation = "none")
fac_load <- fac$loadings
write.csv(fac_load, file = "fac_load.csv")
fac_rotate <- factanal(df1, 4, scores="Bartlett", rotation = "varimax")
fac_rotate_load <- fac_rotate$loadings
write.csv(fac_rotate_load, file = "fac_rotate_load.csv")
