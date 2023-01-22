#The file contains R script for answers to question 1 of
#assignment 2. This is submitted for course work of Statistical
#Structures in Data, PGDBA 08, 2022-23 by Anurag Shukla (22BM6JP08)

#Importing Libraries
library("readxl")
library("dplyr")
library("magrittr")
library("ggpubr")

#Importing the Dataset
setwd("C:/Users/Anurag Shukla/Documents/pgdba/study/ISI/SSD/assignment2")
df <- read_excel("pots_data.xlsx")
df1 <- df[,c(1:9)]

#MDS
distance <- dist(df1)
mds <- df1 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(df1),
          size = 1,
          repel = TRUE)

# Clustering w.r.t region
clust_l <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds_l <- mds %>%
  mutate(groups = clust_l)

# Plot and color by region
ggscatter(mds_l, x = "Dim.1", y = "Dim.2", 
          label = rownames(df1),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

# Plot w.r.t Kiln
plot(mds, pch=16, col=df$Kiln)
title("MDS Plot with Kiln Identification")
