#The file contains R script for answers to question 3 of
#assignment 4. This is submitted for course work of Statistical
#Structures in Data, PGDBA 08, 2022-23 by Anurag Shukla (22BM6JP08)

#Importing Libraries
library("readxl")
library("FactoMineR")
library("ggplot2")
#Importing the Dataset
setwd("C:/Users/Anurag Shukla/Documents/pgdba/study/ISI/SSD/assignment2")
df <- read_excel("tea_data.xlsx", sheet = 1)
#Performing Multiple Correspondence Analysis
tea_mca <- MCA(df, graph = TRUE)
write.csv(tea_mca$eig, file = "mca_eigen.csv")
#Ploting the Data
cats = apply(df, 2, function(x) nlevels(as.factor(x)))
tea_vars_df = data.frame(tea_mca$var$coord, Variable = rep(names(cats), cats))
tea_obs_df = data.frame(tea_mca$ind$coord)
ggplot(data = tea_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(tea_vars_df))) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_text(aes(colour = Variable)) + ggtitle("MCA Plot")

#Tetrachoric Correlation
library("psych")
df1 <- read_excel("tea_data.xlsx", sheet = 2)
tetra_corr <- tetrachoric(df1,y=NULL,correct=TRUE,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE,
            delete=TRUE)
#PCA
corr_mat <- tetra_corr$rho
write.csv(corr_mat, file = "tetra_corr.csv")
eig <- eigen(corr_mat)
eigen_values <- eig$values
#Proportional Variance
propoertion_var <- eigen_values/sum(eigen_values)
pca_scree <- cumsum(propoertion_var)
write.csv(pca_scree, file = "tetra_per_var.csv")
#Scree Plot
plot(pca_scree, xlab = "Principal Component",
     ylab = "Tatal Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot of PCA on Tetrachoric Correlation")
#90% Variance Explained
which(pca_scree >= 0.9)[1]
