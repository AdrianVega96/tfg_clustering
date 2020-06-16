library(readxl) ## Leer datos excel
library(lavaan) ## CFA
library(psych)  ## EFA
library(tidyverse) ## Para el manejo de datos
library(mlogit) ## Regresiones logisticas multinom.
## one-way ANOVA
#library(compute.es); 
#library(car); 
#library(multcomp);
#library(pastecs); ## Estadísticos descriptivos
##
library(vcd)
library(normtest)
## 
rm(list = ls(all = TRUE))
source("functions.R") # Funciones auxiliares

set.seed(12345)

## Datos clusterizados
data_cmeans <- read_excel("Data/dataCmeans.xlsx", sheet = "Datos")
data_kmeans <- read_excel("Data/dataKmeans.xlsx", sheet = "Datos")
data_kmodes <- read_excel("Data/dataKmodes.xlsx", sheet = "Datos")
data_lca <- read_excel("Data/dataLCA.xlsx", sheet = "Datos")

## Elimino observaciones para las que faltan respuestas necesarias
data_cmeans <- drop_na(data_cmeans,30:41,43:53,133:137)
data_kmeans <- drop_na(data_kmeans,30:41,43:53,133:137)
data_kmodes <- drop_na(data_kmodes,30:41,43:53,133:137)
data_lca <- drop_na(data_lca,30:41,43:53,133:137)

## Elimino observaciones para comparación de técnicas
data_lca_comp <- drop_na(data_lca,138:147)
data_kmeans_reorder <- c()
data_kmeans_reorder$Cluster[data_kmeans$Cluster==1] <- 2
data_kmeans_reorder$Cluster[data_kmeans$Cluster==3] <- 1
data_kmeans_reorder$Cluster[data_kmeans$Cluster==2] <- 3

## Comparación mediante tablas cruzadas
cmeans_kmeans_t_r <- table(data_cmeans$Cluster,data_kmeans_reorder$Cluster)
cmeans_kmeans_t <- table(data_cmeans$Cluster,data_kmeans$Cluster)
cmeans_kmodes_t <- table(data_cmeans$Cluster,data_kmodes$Cluster)
cmeans_lca_t <- table(data_cmeans$Cluster,data_lca_comp$Cluster3)
kmeans_kmodes_t <- table(data_kmeans$Cluster,data_kmodes$Cluster)
kmeans_lca_t <- table(data_kmeans$Cluster,data_lca_comp$Cluster3)
lca_kmodes_t <- table(data_lca_comp$Cluster3,data_kmodes$Cluster)

## Concordancia observada y esperada
cmeans_kmeans_agreement <- cbind("Observed" = obs_agreement(cmeans_kmeans_t_r),"Expected" = exp_agreement(cmeans_kmeans_t_r))
cmeans_kmodes_agreement <- cbind("Observed" = obs_agreement(cmeans_kmodes_t),"Expected" = exp_agreement(cmeans_kmodes_t))
cmeans_lca_agreement <- cbind("Observed" = obs_agreement(cmeans_lca_t),"Expected" = exp_agreement(cmeans_lca_t))
kmeans_kmodes_agreement <- cbind("Observed" = obs_agreement(kmeans_kmodes_t),"Expected" = exp_agreement(kmeans_kmodes_t))
kmeans_lca_agreement <- cbind("Observed" = obs_agreement(kmeans_lca_t),"Expected" = exp_agreement(kmeans_lca_t))
lca_kmodes_agreement <- cbind("Observed" = obs_agreement(lca_kmodes_t),"Expected" = exp_agreement(lca_kmodes_t))

## Kappa de Cohen para congruencia entre clasificaciones
k1 <- Kappa(cmeans_kmeans_t_r)
k2 <- Kappa(cmeans_kmodes_t)
k3 <- Kappa(cmeans_lca_t)
k4 <- Kappa(kmeans_kmodes_t)
k5 <- Kappa(kmeans_lca_t)
k6 <- Kappa(lca_kmodes_t)

## Compruebo constructos con alpha de Cronbach
 # Datos de cada constructo
compulsivo <- data_lca[43:53]
exceso <- cbind(data_lca[30:34],data_lca[36])
nodisfrute <- data_lca[37:41]
intenabandonar <- data_lca[133:137]
 # Calculo de alpha
compul_alpha <- psych::alpha(compulsivo)
exceso_alpha <- psych::alpha(exceso)
nodisf_alpha <- psych::alpha(nodisfrute)
intaba_alpha <- psych::alpha(intenabandonar)

## Latent Class Analysis (LCA)
 # CFA (Confirmatory Factor Analysis)
 # Modelo
modelocfa <-"
compulsivo2 =~ v038 +v039 + v040 + v041 + v042 + v043 + v044 + v045 + v046 + v047 + v048
exceso2 =~ v025 +v026 + v027 + v028 + v029 + v031 
nodisfrute2 =~ v032 + v033 + v034 + v035 + v036  
intenabandonar2 =~ v128 + v129 + v130 + v131 +v132 
"

 # CFA
fit <- cfa(modelocfa, data=data_lca, estimator="MLM")
summary(fit, fit.measures=TRUE,  standardized = TRUE, rsquare= T)

 # Predicción valores latentes
valoreslatentes <-lavPredict(fit)
valoreslatentes <- as.data.frame(valoreslatentes)
data_lca <- cbind(data_lca,valoreslatentes)
data_lca$Cluster3 <- as.factor(data_lca$Cluster3)

 # Test de Normalidad de Constructos
 # Intención de abandono composite
normia <- shapiro.test(data_lca$intenabandonar)
 # Intención de abandono factores
normia2 <- shapiro.test(data_lca$intenabandonar2)

 # Multinomial Logistic Regression (MLR)
data_lca_mlr <- mlogit.data(data_lca, choice = "Cluster3", shape = "wide")
lca_mlr_1 <- mlogit(Cluster3 ~ 1 | nodisfrute + compulsivo + exceso,
                  data = data_lca_mlr, reflevel = 2)
summary(lca_mlr_1)
data.frame(exp(lca_mlr_1$coefficients))

lca_mlr_2 <- mlogit(Cluster3 ~ 1 | nodisfrute + compulsivo + exceso,
                  data = data_lca_mlr, reflevel = 1)
summary(lca_mlr_2)
data.frame(exp(lca_mlr_2$coefficients))

 # Kruskal-Wallis de la intencion de abandono
data_lca$Cluster3 <- as.factor(data_lca$Cluster3)
KWia <- kruskal.test(intenabandonar ~ Cluster3, data = data_lca)
KWia
KWia2 <- kruskal.test(intenabandonar2 ~ Cluster3, data = data_lca)
KWia2

 # Regresiones lineales en cada cluster por separado factores
data_lca$Cluster3 <- as.numeric(data_lca$Cluster3)
data_lca_list <- cl_data_list(data_lca)
data_lca_lm <- lm(intenabandonar2 ~ exceso2 + compulsivo2 + nodisfrute2, data = data_lca_list[[1]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar2 ~ exceso2 + compulsivo2 + nodisfrute2, data = data_lca_list[[2]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar2 ~ exceso2 + compulsivo2 + nodisfrute2, data = data_lca_list[[3]])
summary(data_lca_lm)
 # Regresion lineal moderada. Midiendo Cluster como moderador.
data_lca_lm <- lm(intenabandonar2 ~ exceso2 + compulsivo2 + nodisfrute2 +
                  Cluster3_1 + Cluster3_3 + Cluster3_1*compulsivo2 + 
                  Cluster3_3*compulsivo2, data = data_lca)
summary(data_lca_lm)

 # Regresiones lineales en cada cluster por separado composite
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute , data = data_lca_list[[1]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute , data = data_lca_list[[2]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute , data = data_lca_list[[3]])
summary(data_lca_lm)
 # Regresiones lineales moderadas. Midiendo Cluster como moderador.
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute +
                    Cluster3_1 + Cluster3_3 + Cluster3_1*nodisfrute + 
                    Cluster3_3*nodisfrute, data = data_lca)
summary(data_lca_lm)

## C-means (data_cmeans), k-means (data_kmeans) y k-modes (data_kmodes)
datos <- data_kmodes
 # Chi-cuadrado de Pearson
chi_tests <- chisq_items(datos)

 # Calculo los valores latentes de los constructos por factores
datos <- valores_latentes(datos)

 # Test de normalidad constructos por composite
normia <- shapiro.test(datos$intenabandonar)
normco <- shapiro.test(datos$compulsivo)
normex <- shapiro.test(datos$exceso)
normnd <- shapiro.test(datos$nodisfrute)
normtc <- cbind("intenabandono" = normia,"compulsivo" = normco,
                "exceso" = normex,"nodisfrute" = normnd)

 # Test de normalidad constructos por factores
normia <- shapiro.test(datos$intenabandonar2)
normco <- shapiro.test(datos$compulsivo2)
normex <- shapiro.test(datos$exceso2)
normnd <- shapiro.test(datos$nodisfrute2)
normtf <- cbind("intenabandono" = normia,"compulsivo" = normco,
                "exceso" = normex,"nodisfrute" = normnd)

 # Kruskal-Wallis constructos
datos$Cluster <- as.factor(datos$Cluster)
  # Valores latentes por composite
KWia <- kruskal.test(intenabandonar ~ Cluster, data = datos)
KWco <- kruskal.test(compulsivo ~ Cluster, data = datos)
KWex <- kruskal.test(exceso ~ Cluster, data = datos)
KWnd <- kruskal.test(nodisfrute ~ Cluster, data = datos)
KWcomposite <- cbind("intenabandono" = KWia,"compulsivo" = KWco,
                "exceso" = KWex,"nodisfrute" = KWnd)
  # Valores latentes por composite
KWia <- kruskal.test(intenabandonar2 ~ Cluster, data = datos)
KWco <- kruskal.test(compulsivo2 ~ Cluster, data = datos)
KWex <- kruskal.test(exceso2 ~ Cluster, data = datos)
KWnd <- kruskal.test(nodisfrute2 ~ Cluster, data = datos)
KWfactores <- cbind("intenabandono" = KWia,"compulsivo" = KWco,
                 "exceso" = KWex,"nodisfrute" = KWnd)