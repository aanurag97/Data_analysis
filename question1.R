#The file contains R script for answers to question 1 of
#assignment 2. This is submitted for course work of Statistical
#Structures in Data, PGDBA 08, 2022-23 by Anurag Shukla (22BM6JP08)

#Importing the Dataset
library("readxl")
setwd("C:/Users/Anurag Shukla/Documents/pgdba/study/ISI/SSD/assignment2")
df <- read_excel("concrete_dataset.xlsx")

#Computing Dispersion and Correlation Matrices 
S <- cov(df)
R <- cor(df)

#PCA using the Dispersion and Correlation Matrices
pca_S <- prcomp(df, scale = F, retx = T)
pca_R <- prcomp(df, scale = T, retx = T)
summary(pca_S)
summary(pca_R)

#Loadings of both PCA
pca_S_loadings <- pca_S$rotation
pca_R_loadings <- pca_R$rotation

#Variance of Principle Components
pca_S_var <- pca_S$sdev ^ 2
pca_R_var <- pca_R$sdev ^ 2

#Scree Plots for both the PCA
pca_S_scree <- pca_S_var/sum(pca_S_var)
pca_R_scree <- pca_R_var/sum(pca_R_var)
plot(pca_S_scree, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot of Dispersion Matrix")
plot(pca_R_scree, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot of Correlation Matrix")

#Number of Principle Components for 90% variance
which(cumsum(pca_S_scree) >= 0.9)[1]
which(cumsum(pca_R_scree) >= 0.9)[1]

#Plots and tables for report
library(factoextra)
fviz_eig(pca_S)
fviz_eig(pca_R)

#Exporting Data as CSV
write.csv(pca_S_loadings, "pca_S_loading.csv")
write.csv(pca_R_loadings, "pca_R_loading.csv")
write.csv(pca_S_var, "pca_S_var.csv")
write.csv(pca_R_var, "pca_R_var.csv")
