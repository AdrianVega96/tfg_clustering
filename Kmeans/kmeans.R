options(scipen = 999) ## Evitar la notaci?n cient?fica
library(readr) ## Leer datos en .csv
library(tidyverse) ## Para el manejo de datos
library(rowr)
library(ggplot2)
library(factoextra)
library(xlsx)
library(readxl)
rm(list = ls(all = TRUE))
source("functions.R") # Funciones auxiliares

set.seed(12345)

data <- read.csv("BDlimpia.csv", header = TRUE, sep = ";")
constructos <- data_cmeans <- read_excel("valoreslatentes.xlsx", sheet = "vlatentes")
my_data <- data_pre(data)
my_data <- cbind(my_data,constructos)
my_data <- drop_na(my_data,137:146)

cluster_items <- my_data[137:146]

## Transformaci?n de solo las variables categ?ricas mediante one-hot, las ordinales
 # se mantienen igual
datao <- one_hot(cluster_items[1:4])
datao <- cbind(datao,cluster_items[5:10])
mod_sel_graphs <- selection_graphs(datao,1)
ggsave("Kmeans/elbow-kmeans.png", mod_sel_graphs[[1]], width = 7.5, height = 5, units = "in")
ggsave("Kmeans/sil-kmeans.png", mod_sel_graphs[[2]], width = 7.5, height = 5, units = "in")
ggsave("Kmeans/gap-kmeans.png", mod_sel_graphs[[3]], width = 7.5, height = 5, units = "in")

model <- kmeans(datao,3)

## Graficos para el modelo final
 # Separo las observaciones de cada cluster
dataux <- as.data.frame(cbind(my_data,"Cluster" = model$cluster))
datac1 <- dataux[dataux$Cluster==1,]
datac2 <- dataux[dataux$Cluster==2,]
datac3 <- dataux[dataux$Cluster==3,]

pie <- dist_graph(model$cluster)
pie
ggsave("Kmeans/proportions.png", pie, width = 7.5, height = 5, units = "in")
plt1 <- response_graph(datac1[137:146],colnames(my_data[137:146]))
plt1
ggsave("Kmeans/responsesc1.png", plt1, width = 7.5, height = 5, units = "in")
plt2 <- response_graph(datac2[137:146],colnames(my_data[137:146]))
plt2
ggsave("Kmeans/responsesc2.png", plt2, width = 7.5, height = 5, units = "in")
plt3 <- response_graph(datac3[137:146],colnames(my_data[137:146]))
plt3
ggsave("Kmeans/responsesc3.png", plt3, width = 7.5, height = 5, units = "in")
## Guardar datos para análisis posteriores

write.xlsx(dataux,"Data/dataKmeans.xlsx",sheetName = "Datos")
