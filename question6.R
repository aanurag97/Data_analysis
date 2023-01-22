#The file contains R script for answers to question 1 of
#assignment 2. This is submitted for course work of Statistical
#Structures in Data, PGDBA 08, 2022-23 by Anurag Shukla (22BM6JP08)

#Importing Libraries
library("readxl")
library("MASS")

#Importing the Dataset
setwd("C:/Users/Anurag Shukla/Documents/pgdba/study/ISI/SSD/assignment2")
df <- read_excel("flowers_dataset.xlsx")
df1 <- as.matrix(df[,-1])

#Non-Metric MDS with two Dimensions
flowers <- isoMDS(df1, k = 2)
plot(flowers$points, type = "n")
text(flowers$points, labels = as.character(1:nrow(df)))
title("Non-Metric MDS")

#kruskal's Stress Scree Plot
stress <- c()
for (x in 1:9) {
  mds <- isoMDS(df1, k = x)$stress
  stress <- append(stress, mds)
}

#Scree Plot for Krushkal's Stress
stress <- stress/sum(stress)
x <- seq(1,9)
plot(x,stress)
title("Kruskal's Stress Index")

#Goodness of Association for 2 Dimensions
stress_2dim <- stress[2]