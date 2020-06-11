options(scipen = 999) ## Evitar la notaci?n cient?fica
library(readr) ## Leer datos en .csv
library(tidyverse) ## Para el manejo de datos
library(rowr)
library(ztable) ## Tabla por viewer
library(ggplot2) ## Gráficos
library(e1071) ## Función cmeans
library(xlsx) ## Crear fichero .xlsx con las observaciones clusterizadas
library(readxl)
rm(list = ls(all = TRUE))
source("functions.R") # Funciones auxiliares
set.seed(12345)

## Datos
data <- read.csv("BDlimpia.csv", header = TRUE, sep = ";")
constructos <- data_cmeans <- read_excel("valoreslatentes.xlsx", sheet = "vlatentes")
my_data <- data_pre(data)
my_data <- cbind(my_data,constructos)
my_data <- drop_na(my_data,137:146)

cluster_items <- my_data[137:146]

datao <- one_hot(cluster_items[1:4])
datao <- cbind(datao,cluster_items[5:10])
## Estimo modelos con las variables con representaci?n one-hot y creo tabla para
 # seleccionar el mejor modelo
model_selection_table(datao)

model <- cmeans(datao,3,FALSE,iter.max = 100,dist = "euclidean",method = "cmeans",1.1)

## Gráficos para el modelo final
 # Separo las observaciones de cada cluster
dataux <- as.data.frame(cbind(my_data,"Cluster" = model$cluster))
datac1 <- dataux[dataux$Cluster==1,]
datac2 <- dataux[dataux$Cluster==2,]
datac3 <- dataux[dataux$Cluster==3,]

pie <- dist_graph(model$cluster)
pie
ggsave("Cmeans/proportions.png", pie, width = 7.5, height = 5, units = "in")
plt1 <- response_graph(datac1[137:146],colnames(my_data)[137:146])
plt1
ggsave("Cmeans/responsesc1.png", plt1, width = 7.5, height = 5, units = "in")
plt2 <- response_graph(datac2[137:146],colnames(my_data)[137:146])
plt2
ggsave("Cmeans/responsesc2.png", plt2, width = 7.5, height = 5, units = "in")
plt3 <- response_graph(datac3[137:146],colnames(my_data)[137:146])
plt3
ggsave("Cmeans/responsesc3.png", plt3, width = 7.5, height = 5, units = "in")

## Guardar datos para análisis posteriores

write.xlsx(dataux,"Data/dataCmeans.xlsx",sheetName = "Datos")
