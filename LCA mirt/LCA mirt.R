options(scipen = 999) ## Evitar la notaci?n cient?fica
library(readr) ## Leer datos en .csv
library(xlsx)
library(poLCA) ## Funci?n para simular datos en funci?n de par?metros
library(mirt)
library(tidyverse) ## Para el manejo de datos
library(ztable)
library(doParallel)
library(readxl)
source("functions.R") # Funciones auxiliares

set.seed(12345)

data <- read.csv("BDlimpia.csv", header = TRUE, sep = ";")
constructos <- data_cmeans <- read_excel("valoreslatentes.xlsx", sheet = "vlatentes")
my_data <- data_pre(data)
my_data <- cbind(my_data,constructos)
cluster_items <- my_data[137:146]

model1 <- mdirt(cluster_items, 2, itemtype = "lca", verbose = FALSE)
model2 <- mdirt(cluster_items, 3, itemtype = "lca", verbose = FALSE)
model3 <- mdirt(cluster_items, 4, itemtype = "lca", verbose = FALSE)
model4 <- mdirt(cluster_items, 5, itemtype = "lca", verbose = FALSE)
model5 <- mdirt(cluster_items, 6, itemtype = "lca", verbose = FALSE)

lcas <- list(model1,model2,model3,model4,model5)
ctable(lcas,500)

classnames <- c('1','2','3','4','5')
itemnames <- c('Sexo','ECivil','NFamiliar','PriEmp','Edad','Exp','NEmple','NSocios',
               'NHijos','NivelEd')
mult.param <- compgraph(model2,3,classnames)
ggsave("LCA mirt/responses.png", mult.param, width = 7.5, height = 5, units = "in")
class.prop <- distgraph(model2,3,classnames)
ggsave("LCA mirt/proportions.png", class.prop, width = 7.5, height = 5, units = "in")

mod.groups <- cbind("Cluster2" = modalassign(fscores(model1)),"error2" = classError(model1)[[1]],
                    dummyVar(modalassign(fscores(model1)),"Cluster2"),
                    "Cluster3" = modalassign(fscores(model2)),"error3" = classError(model2)[[1]],
                    dummyVar(modalassign(fscores(model2)),"Cluster3"))
reg.data <- cbind(my_data,mod.groups)
write.xlsx(reg.data,"Data/dataLCA.xlsx",sheetName = "Datos")