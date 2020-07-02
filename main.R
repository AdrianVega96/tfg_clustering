library(readxl) # Leer datos excel
library(psych)  # Alpha de Cronbach
library(tidyverse) # Para el manejo de datos
library(mlogit) # Regresiones logisticas multinom.
library(vcd) # Kappa de Cohen
library(normtest) # Test de normalidad
rm(list = ls(all = TRUE))
unlink("DatosClusterizados.xlsx")
source("functions.R") # Funciones auxiliares
set.seed(12345)

# Se leen los datos. Categoriza items para clusterizar y les asigna nombre.
# Finalmente se añade a los datos originales estos items.
data <- read.csv("BDlimpia.csv", header = TRUE, sep = ";")
my_data <- data_preparation(data)
constructos_composite <- read_excel("valoreslatentes.xlsx", sheet = "vlatentes")
my_data <- cbind(my_data,constructos_composite)
constructos_factores <- valores_latentes_cfa(my_data)

# Se cluzterizan los datos y los datos clusterizados se guardan en un dataframe
# diferente en función del algoritmo empleado.
data_lca <- My_lca(my_data)
data_cmeans <- My_cmeans(my_data)
data_kmeans <- My_kmeans(my_data)
data_kmodes <- My_kmodes(my_data)

# Añadir constructos obtenidos mediante CFA a los datos clusterizados. Es necesario
# eliminar las observaciones para las que no es posible su calculo de manera que
# las observaciones se correspondan con los valores latentes de estos constructos.
vl <- drop_na(my_data,29:33,35:40,42:52,132:136)
vl_lca <- cbind(vl,constructos_factores)
vl_k <- drop_na(vl_lca,137:146)

# Elimino observaciones para las que faltan respuestas necesarias por el mismo motivo.
data_cmeans <- drop_na(data_cmeans,29:33,35:40,42:52,132:136)
data_kmeans <- drop_na(data_kmeans,29:33,35:40,42:52,132:136)
data_kmodes <- drop_na(data_kmodes,29:33,35:40,42:52,132:136)
data_lca <- drop_na(data_lca,29:33,35:40,42:52,132:136)

 # Añado finalmente los valores latentes a los datos clusterizados.
data_cmeans <- cbind(data_cmeans,"compulsivo_cfa"=vl_k$compulsivo_cfa,"exceso_cfa"=vl_k$exceso_cfa,
                     "nodisfrute_cfa"=vl_k$nodisfrute_cfa,"intenabandonar_cfa"=vl_k$intenabandonar_cfa)
data_kmeans <- cbind(data_kmeans,"compulsivo_cfa"=vl_k$compulsivo_cfa,"exceso_cfa"=vl_k$exceso_cfa,
                     "nodisfrute_cfa"=vl_k$nodisfrute_cfa,"intenabandonar_cfa"=vl_k$intenabandonar_cfa)
data_kmodes <- cbind(data_kmodes,"compulsivo_cfa"=vl_k$compulsivo_cfa,"exceso_cfa"=vl_k$exceso_cfa,
                     "nodisfrute_cfa"=vl_k$nodisfrute_cfa,"intenabandonar_cfa"=vl_k$intenabandonar_cfa)
data_lca <- cbind(data_lca,"compulsivo_cfa"=vl_lca$compulsivo_cfa,"exceso_cfa"=vl_lca$exceso_cfa,
                  "nodisfrute_cfa"=vl_lca$nodisfrute_cfa,"intenabandonar_cfa"=vl_lca$intenabandonar_cfa)

# Elimino observaciones para comparación de técnicas. Ya que LCA maneja la falta de respuestas
# pero K-means, C-means y K-modes no.
data_lca_comp <- drop_na(data_lca,137:146)
# Reordeno los clústeres debido a la similitud entre el resultado de K-means y C-means.
data_kmeans_reorder <- c()
data_kmeans_reorder$Cluster[data_kmeans$Cluster==1] <- 2
data_kmeans_reorder$Cluster[data_kmeans$Cluster==3] <- 1
data_kmeans_reorder$Cluster[data_kmeans$Cluster==2] <- 3

# Comparación mediante tablas cruzadas
cmeans_kmeans_t_r <- table(data_cmeans$Cluster,data_kmeans_reorder$Cluster)
cmeans_kmeans_t <- table(data_cmeans$Cluster,data_kmeans$Cluster)
cmeans_kmodes_t <- table(data_cmeans$Cluster,data_kmodes$Cluster)
cmeans_lca_t <- table(data_cmeans$Cluster,data_lca_comp$Cluster)
kmeans_kmodes_t <- table(data_kmeans$Cluster,data_kmodes$Cluster)
kmeans_lca_t <- table(data_kmeans$Cluster,data_lca_comp$Cluster)
lca_kmodes_t <- table(data_lca_comp$Cluster,data_kmodes$Cluster)

# Concordancia observada y esperada
cmeans_kmeans_agreement <- cbind("Observed" = obs_agreement(cmeans_kmeans_t_r),"Expected" = exp_agreement(cmeans_kmeans_t_r))
cmeans_kmodes_agreement <- cbind("Observed" = obs_agreement(cmeans_kmodes_t),"Expected" = exp_agreement(cmeans_kmodes_t))
cmeans_lca_agreement <- cbind("Observed" = obs_agreement(cmeans_lca_t),"Expected" = exp_agreement(cmeans_lca_t))
kmeans_kmodes_agreement <- cbind("Observed" = obs_agreement(kmeans_kmodes_t),"Expected" = exp_agreement(kmeans_kmodes_t))
kmeans_lca_agreement <- cbind("Observed" = obs_agreement(kmeans_lca_t),"Expected" = exp_agreement(kmeans_lca_t))
lca_kmodes_agreement <- cbind("Observed" = obs_agreement(lca_kmodes_t),"Expected" = exp_agreement(lca_kmodes_t))

# Kappa de Cohen para congruencia entre clasificaciones
k1 <- Kappa(cmeans_kmeans_t_r)
k2 <- Kappa(cmeans_kmodes_t)
k3 <- Kappa(cmeans_lca_t)
k4 <- Kappa(kmeans_kmodes_t)
k5 <- Kappa(kmeans_lca_t)
k6 <- Kappa(lca_kmodes_t)

# Compruebo constructos con alpha de Cronbach
# Datoframe para el cálculo de cada constructo
compulsivo <- data_lca[42:52]
exceso <- cbind(data_lca[29:33],data_lca[35])
nodisfrute <- data_lca[36:40]
intenabandonar <- data_lca[132:136]
# Calculo de alpha
compul_alpha <- psych::alpha(compulsivo)
exceso_alpha <- psych::alpha(exceso)
nodisf_alpha <- psych::alpha(nodisfrute)
intaba_alpha <- psych::alpha(intenabandonar)

data_lca$Cluster <- as.factor(data_lca$Cluster)

# Test de Normalidad de Intención de Abandono en LCA
# Intención de abandono composite
normia_lca <- shapiro.test(data_lca$intenabandonar)
# Intención de abandono factores
normia_lca_cfa <- shapiro.test(data_lca$intenabandonar_cfa)

# Kruskal-Wallis de la intencion de abandono
# Constructos por factores
KWia_lca_cfa <- kruskal.test(intenabandonar_cfa ~ Cluster, data = data_lca)
KWia_lca_cfa
# Constructos por composite
KWia_lca <- kruskal.test(intenabandonar ~ Cluster, data = data_lca)
KWia_lca

# Regresión Logística Multinomial (MLR) para constructos por factores
# Segundo clúster como referencia
data_lca_mlr <- mlogit.data(data_lca, choice = "Cluster", shape = "wide")
lca_mlr_1 <- mlogit(Cluster ~ 1 | nodisfrute_cfa + compulsivo_cfa + exceso_cfa,
                  data = data_lca_mlr, reflevel = 2)
summary(lca_mlr_1)
data.frame(exp(lca_mlr_1$coefficients))
# Primer clúster como referencia
lca_mlr_2 <- mlogit(Cluster ~ 1 | nodisfrute_cfa + compulsivo_cfa + exceso_cfa,
                  data = data_lca_mlr, reflevel = 1)
summary(lca_mlr_2)
data.frame(exp(lca_mlr_2$coefficients))

# Regresión Logística Multinomial (MLR) para constructos por composite
# Segundo clúster como referencia
data_lca_mlr <- mlogit.data(data_lca, choice = "Cluster", shape = "wide")
lca_mlr_1 <- mlogit(Cluster ~ 1 | nodisfrute + compulsivo + exceso,
                    data = data_lca_mlr, reflevel = 2)
summary(lca_mlr_1)
data.frame(exp(lca_mlr_1$coefficients))
# Primer clúster como referencia
lca_mlr_2 <- mlogit(Cluster ~ 1 | nodisfrute + compulsivo + exceso,
                    data = data_lca_mlr, reflevel = 1)
summary(lca_mlr_2)
data.frame(exp(lca_mlr_2$coefficients))

# Regresiones lineales en cada cluster por separado factores
# Constructos obtenidos por factores
data_lca$Cluster <- as.numeric(data_lca$Cluster)
data_lca_list <- cl_data_list(data_lca)
data_lca_lm <- lm(intenabandonar_cfa ~ exceso_cfa + compulsivo_cfa + nodisfrute_cfa, data = data_lca_list[[1]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar_cfa ~ exceso_cfa + compulsivo_cfa + nodisfrute_cfa, data = data_lca_list[[2]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar_cfa ~ exceso_cfa + compulsivo_cfa + nodisfrute_cfa, data = data_lca_list[[3]])
summary(data_lca_lm)
# Regresion lineal moderada. Midiendo Cluster como moderador.
data_lca_lm <- lm(intenabandonar_cfa ~ exceso_cfa + compulsivo_cfa + nodisfrute_cfa +
                  Cluster_1 + Cluster_3 + Cluster_1*compulsivo_cfa + 
                  Cluster_3*compulsivo_cfa, data = data_lca)
summary(data_lca_lm)

# Regresiones lineales en cada cluster por separado composite
# Constructos obtenidos por composite
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute , data = data_lca_list[[1]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute , data = data_lca_list[[2]])
summary(data_lca_lm)
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute , data = data_lca_list[[3]])
summary(data_lca_lm)
# Regresiones lineales moderadas. Midiendo Cluster como moderador.
data_lca_lm <- lm(intenabandonar ~ exceso + compulsivo + nodisfrute +
                    Cluster_1 + Cluster_3 + Cluster_1*nodisfrute + 
                    Cluster_3*nodisfrute, data = data_lca)
summary(data_lca_lm)

# C-means (data_cmeans), k-means (data_kmeans) y k-modes (data_kmodes)
datos <- list(data_cmeans,data_kmeans,data_kmodes)
names(datos) <- c("cmeans","kmeans","kmodes")

# Test de normalidad constructos por composite
normia <- shapiro.test(datos[[1]]$intenabandonar)
normco <- shapiro.test(datos[[1]]$compulsivo)
normex <- shapiro.test(datos[[1]]$exceso)
normnd <- shapiro.test(datos[[1]]$nodisfrute)
normtc <- cbind("intenabandono" = normia,"compulsivo" = normco,
                "exceso" = normex,"nodisfrute" = normnd)

# Test de normalidad constructos por factores
normia <- shapiro.test(datos[[1]]$intenabandonar_cfa)
normco <- shapiro.test(datos[[1]]$compulsivo_cfa)
normex <- shapiro.test(datos[[1]]$exceso_cfa)
normnd <- shapiro.test(datos[[1]]$nodisfrute_cfa)
normtf <- cbind("intenabandono" = normia,"compulsivo" = normco,
                "exceso" = normex,"nodisfrute" = normnd)

# Chi-cuadrado de Pearson de items para clusterizar
chi_tests <- chi_squared_items(datos)

# Kruskal-Wallis constructos
KW_tests <- kruskal_test(datos)