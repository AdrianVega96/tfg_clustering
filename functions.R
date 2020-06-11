## Análisis de Clases Latentes (LCA)
## Funcion para la entropia
 # mod es el modelo de clases latentes para el cual se calcula la entropía
entropy <- function(mod){
  en<--sum(fscores(mod)*log(fscores(mod)),na.rm = TRUE)
  e<-1-en/(nrow(fscores(mod))*log(ncol(fscores(mod))))
  return(e)
}

## Funci?n para categorizar las variables no categoricas y simplificar
  # las categ?ricas.
data_pre <- function(data){
  ## Los -1 que indican la falta del dato los modifico por NA
  data[data<0] <- NA
  datafi <- data
  ##("S","AN","EsCi","NHij","Educ","PriEmp","Exp","NegFam","NEmpl","NSocios")
  data <- data %>% select(v001,v002,v003,v004,v005,v007,v009,v010,v012,v013)
  ## Calculo la edad de cada encuestado
  edad <- 2019-data$v002
  ## La categorizo. Menor de 41 a?os (1), entre 41 y 60 (2) y m?s de 60 (3)
  edcat <- edad
  edcat[edcat<=40] <- 1
  edcat[edcat<=60 & edcat>40] <- 2
  edcat[edcat>60 & edcat<=120] <- 3
  ## Categorizo la experiencia. Menor de 6 a?os (1), entre 6 y 15 a?os (2) y
  # mayor de 16 a?os (3)
  expcat <- data$v009
  expcat[expcat<6] <- 1
  expcat[expcat>=6 & expcat<16] <- 2
  expcat[expcat>=16] <- 3
  ## Categorizo el n?mero de empleados. Menos de 6 (1), entre 6 y 20 (2) y
  # m?s de 20 (3)
  nempcat <- data$v012
  nempcat[nempcat<=5] <- 1
  nempcat[nempcat>5 & nempcat<=20] <- 2
  nempcat[nempcat>20] <- 3
  ## Categorizo el n?mero de socios. Sin socios (1) y con socios (2)
  nsocat <- data$v013
  nsocat[nsocat>=1] <- 2
  nsocat[nsocat==0] <- 1
  ## Categorizo el n?mero de hijos. Sin hijos o con menos de 3 hijos (1) y
  # con 3 hijos o m?s (2)
  nhijcat <- data$v004
  nhijcat[nhijcat==0 & nhijcat<=2] <- -1
  nhijcat[nhijcat>2] <- 2
  nhijcat[nhijcat==-1] <- 1
  ## Categorizo el nivel educativo. Estudios primarios (1), Bachillerato/ciclo (2) y Estudios universitarios (3)
  educat <- data$v005
  educat[educat<=3] <- 1
  educat[educat>3 & educat<=5] <- 2
  educat[educat>5] <- 3
  ## Elimino valores extra?os
  data$v003[data$v003==0] <- NA
  data$v003[data$v003>20] <- NA
  ## Sumo uno a las variables con 0 porque la librer?a no los acepta.
  data$v010 <- data$v010+1
  data$v007 <- data$v007+1
  ## Genero el dataframe con las nuevas variables categ?ricas y le
  # asigno nombre las columnas.
  data2 <- cbind(datafi,data$v001,data$v003,data$v010,
                 data$v007,edcat,expcat,nempcat,nsocat,nhijcat,
                 educat)
  colnames(data2) <- c(colnames(datafi),'Sexo','ECivil','NFamiliar','PriEmp','Edad',
                       'Exp','NEmple','NSocios','NHijos','NivelEd')
  return(data2)
}
## Crear dummys para regresiones y como auxiliar para la codificación one-hot
 # dataCol columna de datos que se corresponde con la variable para la que 
 # se requieren dummys. 
 # nombre es el nombre de la variable, se utiliza para generar un nombre del 
 # estilo “nombre_número”
dummyVar <- function(dataCol,nombre){
  # Se inicializan las variables auxiliares necesarias
  aux <- c() # Dummy
  res <- c() # Resultado
  cnames <- c() # Nombres de columnas
  # Se recorre un bucle que itera tantas veces como indica el mayor 
  # valor de la columna de datos
  for (i in 1:max(dataCol)){
    # Se recorre la columna de datos para generar la dummy, es decir, 
    # 1 o 0 cuando se cumpla la condición
    for (e in 1:length(dataCol)){
      if (dataCol[e]==i){
        aux[e] <- 1
      }else{
        aux[e] <- 0
      }
    }
    # Se va generando el dataframe final uniendo las dummys por columnas
    res <- cbind(res,aux)
    # Se genera la lista de nombres de columnas para asignarlos después
    cnames <- c(cnames,paste0(nombre,"_",i))
  }
  colnames(res) <- cnames
  return (res)
}

## Funci?n para el c?lculo de la probabilidad de error de clasificaci?n
# de cada observaci?n, as? como el total de todo el modelo.
# Hace uso del modelo en cuesti?n.
classError <- function(mod){
  errors <- c()
  # Probabilidades de asignaci?n de cada observaci?n.
  probs <- fscores(mod)
  for (i in 1:nrow(probs)){
    # Para cada observaci?n, la probabilidad de error es 
    # 1 menos la probabilidad de asignaci?n mayor, es decir,
    # entendemos que se produce un error cuando no se asigna
    # al grupo de mayor probabilidad.
    errors[i] <- (1 - max(probs[i,]))
  }
  # Se calcula la proporci?n total de errores del modelo
  totError <- sum(errors)/length(errors)
  # Se crea una lista con ambos valores de inter?s para devolvera como resultado.
  res <- list(errors,totError)
  return (res)
}

## Calculo el BLRT para cada par de modelos posible, es decir,
# para todos los modelos con K y K+1 clases. 
# Se realiza el bootstrap con nboot muestras diferentes, en caso de no
# especificarse el bootstrap se realizar? con 500 muestras.
bootlrt <- function(lcas,nboot=NULL){
  # Vector resultado inicializado con un 0, puesto que no existe comparaci?n
  # entre un modelo de 1 y 2 clusters.
  pValues <- c(0)
  # Creo distintas sesiones de R para la ejecuci?n en paralelo.
  cl <- makeCluster(detectCores()-1)
  # Ejecuto las siguientes instrucciones en cada una de las sesiones.
  clusterEvalQ(cl,source("functions.R"))
  clusterEvalQ(cl,library(poLCA))
  clusterEvalQ(cl,library(mirt))
  # Ejecuto cada iteraci?n del bucle en paralelo y combino los resultados en
  # un vector. De esta manera, el tiempo de ejecuci?n se reduce a menos de la
  # mitad del tiempo original.
  registerDoParallel(cl)
  aux <- foreach (i=1:(length(lcas)-1), .combine = "c") %dopar% {
    # Ejecuto la funci?n que calcula el estad?stico para cada par de modelos.
    bootlrtaux(lcas[[i]],lcas[[i+1]],nboot)
  }
  # Cierro las distintas sesiones de R
  stopCluster(cl)
  # Combino los resultados obtenidos con el vector resultado.
  pValues <- c(pValues,aux)
  return(pValues)
}

## Calcula el Bootstrap Likelihood Ratio Test (BLRT) entre dos modelos estimados.
# nboot es el número de muestras de bootstrap. 500 por defecto.
# mod0 es el primer modelo
# mod1 es el segundo modelo (con más clases latentes)
bootlrtaux <- function(mod0,mod1, nboot=500){
  lrt0 <- anova(mod0,mod1,verbose=FALSE)$X2[2] # Calcula LRT
  cont <- 0 # Contador para el cálculo del p valor (Proporción de LRT mayores o iguales al original)
  tot <- 0 # Contador de modelos que convergen, es decir, que se maximiza la función de log-likelihood.
  for (i in 1:nboot){
    # Creo una muestra del tamaño de la orginal a partir de las probabilidades condicionales y
    # las probabilidades a priori, es decir, la proporción de observaciones que pertenece a cada
    # cluster.
    datsim <- poLCA.simdata(N=NROW(fscores(mod0)),probs = summary(mod0)[1:10],P = summary(mod0)[[11]][,ncol(summary(mod0)[[11]])])[[1]]
    # Estimo dos nuevos modelos con las misma condiciones que los originales con la nueva muestra.
    modellr <- mdirt(datsim,NCOL(fscores(mod0)),itemtype = "lca",verbose = FALSE, technical = list(warn=FALSE, message=FALSE))
    modelmr <- mdirt(datsim,NCOL(fscores(mod1)),itemtype = "lca",verbose = FALSE, technical = list(warn=FALSE, message=FALSE))
    if (!extract.mirt(modellr, "converged")) next
    if (!extract.mirt(modelmr, "converged")) next
    tot <- tot + 1
    # Vuelvo a calcular el estad?stico LRT con las nuevos modelos, pero solo si el modelo converge.
    # lliklr <- extract.mirt(modellr, "logLik")
    # llikmr <- extract.mirt(modelmr, "logLik")
    # lrt <- -2*(lliklr-llikmr)
    lrt <- anova(modellr,modelmr,verbose=FALSE)$X2[2]
    # Incremento el contador si se cumple la condici?n mencionada anteriormente
    if(lrt>=lrt0) cont <- cont+1
  }
  # Calculo el p valor como la proporci?n del estad?stico que cumple la condici?n.
  pvalue <- (1+cont)/(1+nboot)
  #pvalue <- (1+cont)/(1+tot)
  return(pvalue)
}

## Crear tablas para la selecci?n del modelo.
# lcas es una lista con los modelos estimados.
# nboot es el número de muestras de bootstrap para BLRT
ctable <- function(lcas,nboot=NULL){
  #Se crean vectores con los estad?sticos calculados para cada modelo
  bics <- sapply(lcas,function(x) extract.mirt(x,"BIC"))
  aics <- sapply(lcas,function(x) extract.mirt(x,"AIC"))
  llik <- sapply(lcas,function(x) extract.mirt(x,"logLik"))
  SABIC <- sapply(lcas,function(x) extract.mirt(x,"SABIC"))
  AICc <- sapply(lcas,function(x) extract.mirt(x,"AICc"))
  npar <- sapply(lcas,function(x) extract.mirt(x,"nest"))
  entropy <- sapply(lcas, entropy)
  pBLRT <- bootlrt(lcas,nboot)
  clErr <- sapply(lcas,function(x) classError(x)[[2]])
  # Se unen los vectores en un dataframe por columnas
  Ktable <- as.data.frame(cbind(llik,npar,bics,SABIC,aics,AICc,entropy,pBLRT,clErr))
  # Se asignan nombres de filas y columnas
  rownames(Ktable) <- c(2:(length(lcas)+1))
  tcolnames <- c('Log-Lik','# Param','BIC','SABIC','AIC','AICc','Entropy','BLRT','Class.Err.')
  colnames(Ktable) <- tcolnames
  Ktable <- round(Ktable, 2)
  Ktable[1,8] <- "-"
  # Se muestra tabla por el viewer de RStudio
  ztable(Ktable)
}

## Tratamiento de los resultados para realizar las gr?ficas.
datatreat <- function(probs=NULL, ncl=NULL, clnames=NULL, predcl=NULL, classDist=NULL){
  ## Compruebo si es el gr?fico sobre la distribuci?n de los individuos en grupos
  if (!is.null(predcl)){
    predclass <- predcl
    for (i in 1:ncl){
      predclass[predclass==i] <- paste0(clnames[i]," ","(",round(classDist[i]*100,2),"%",")")
    }
    return(predclass)
  }
  ## Convierto las matrices de probabilidades en un dataframe
  # para poder darle el formato que necesito para graficarlos
  probs.df <- as.data.frame(probs)
  ## Obtengo los nombres de las columnas
  colnames2 <- names(probs.df)
  ## Pivoto para a?adir la informaci?n de las columnas por debajo en las filas
  # de manera parecida a la transposici?n de una matriz (Se pierde informaci?n)
  # sobre las clases
  probs2 <-  probs.df%>%pivot_longer(cols = colnames2, values_to = "value")
  ## Creo un vector con los nombres de los grupos para a?adir al dataframe.
  # Lo necesitar? m?s adelante ya que perder? esta informaci?n en el proceso.
  classvector <- c()
  for(i in 1:ncl){
    #classvector <- c(rep('Class 1', nind*nres),rep('Class 2', nind*nres),rep('Class 3', nind*nres))
    classvector <- c(classvector, rep(clnames[i], nrow(probs2)/ncl))
  }
  probs2$class <- classvector
  ## Trato la columna 'name'
  # Tiene el siguiente formato 'ECivil.category_1', por lo que
  # ... separo la columna en dos a partir de '.', en item incluyo
  # el indicador y en response la respuesta, en el ejemplo, 'ECivil' a
  # 'item' y 'Category_1' a 'response'.
  probs2 <- separate(probs2, col = 'name', into = c('item','response'),sep = '\\.')
  probs2$response <- gsub("category_","",probs2$response)
  return(probs2)
}

## Realiza el gr?fico sobre los patrones de respuesta de cada clase, tambi?n
# llamadas probabilidades condicionales P(X=t|Y).
# Hace uso del modelo estimado, el n?mero de clusters y los nombres de 
# los clusters.
compgraph <- function(mod,ncl,clnames){
  probs <- datatreat(summary(mod)[1:10],ncl,clnames)
  plt <- ggplot(probs)
  # A?ado los ejes x e y, adem?s especif?co que se trata de un gr?fico de columnas
  plt <-  plt + aes(x = probs$response, y = probs$value) + geom_col() +
    # A?ado la faceta, me permite repetir el gr?fico para cada indicador de cada
    # clase y le a?ado el formato adecuado para su mejor lectura
    facet_grid(probs$class~factor(probs$item, levels = itemnames))+
    theme(text=element_text(size=8)) +
    # A?ado la escala del eje Y (son probabilidades) de 0 a 1.
    scale_y_continuous(breaks=c(0.5,1),labels=c(0.5,1)) +
    xlab('Respuesta')+ylab('Probabilidad')
  plt
}

## Realiza el gr?fico circular sobre el porcentaje de individuos que hay en 
# cada clase.
# Hace uso del modelo estimado, del n?mero de clusters y de los nombres
# de cada cluster.
distgraph <- function(mod, nclu, classnames){
  ## Tratamiento previo de la informaci?n a graficar
  aux2 <- datatreat(ncl = nclu, predcl = modalassign(fscores(mod)), clnames = classnames, classDist = summary(mod)$Class.Probability$prob)
  ## Gr?fico circular
  pie <- ggplot(as.data.frame(modalassign(fscores(mod))), aes(x = "", fill = factor(aux2))) + 
    geom_bar(width = 1) + coord_polar(theta = "y") +
    labs(fill="Clase", 
         x=NULL, 
         y=NULL, 
         title="Proporciones de clase") + theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  pie
}

## Funci?n que realiza la asignaci?n modal de las observaciones.
# Hacer uso de las probabilidades de pertenecer a cada cluster.
modalassign <- function(probs){
  res <- c()
  ## Para cada fila de probabilidades, que se corresponde a cada observaci?n
  for (i in 1:nrow(probs)){
    ## Concateno al resultado la posici?n de la mayor probabilidad, es decir,
    # si la mayor probabilidad es la primera, se asigna al primer grupo.
    res <- c(res,which.max(probs[i,]))
  }
  return(as.vector(res))
}

## K-means, K-modes y C-means

## Funci?n que tranforma un conjunto de variables a one-hot
one_hot <- function (data){
  res <- c()
  for (i in 1:ncol(data)){
    res <- cbind(res,dummyVar(data[,i],colnames(data)[i]))
  }
  return(res)
}

## Funci?n para obtener la frecuencia de aparici?n de las respuestas en un cluster
cluster_responses <- function(data,cnames){
  res <- c()
  for (i in 1:ncol(data)){
    a <- tabulate(data[,i])
    aux <- a/sum(a)
    res <- cbind.fill(res,aux,fill = 0)
  }
  res <- res[2:11]
  colnames(res) <- cnames
  return(res)
}

## Funci?n para obtener un gr?fico con la frecuencia de aparici?n de las respuestas 
# en un cluster concreto.
response_graph <- function(data,cnames){
  freq <- cluster_responses(data,cnames)
  item <- c(rep(1,length(cnames)),rep(2,length(cnames)),rep(3,length(cnames)))
  graph_data <- freq%>%pivot_longer(cols = colnames(freq), values_to = "value")
  graph_data <- cbind(graph_data,item)
  plt <- ggplot(graph_data)
  # A?ado los ejes x e y, adem?s especif?co que se trata de un gr?fico de columnas
  plt <-  plt + aes(x = graph_data$item, y = graph_data$value) + geom_col() +
    facet_grid(factor(graph_data$name, levels = cnames))+
    theme(text=element_text(size=8)) +
    # A?ado la escala del eje Y (frecuencias) de 0 a 1.
    scale_y_continuous(breaks=c(0.5,1),labels=c(0.5,1)) +
    xlab('Respuesta')+ylab('Probabilidad')
  plt
}

MyKmeansFUN <- function(x,k){
  kmeans(x, k, iter.max=50)
}
## Función que genera los gráficos de los distintos métodos de selección
 # del número de clústeres
 # data es dataframe con los items utilizados para clusterizar
 # i es una variable que me permite pivotar entre los algoritmos kmeans (1) y kmodes (2)
 # para generar los gráficos para el que corresponde. Kmeans por defecto
selection_graphs <- function(data,i=1){
  if (i==1){
    # Método elbow para kmeans
    a <- fviz_nbclust(data, kmeans, method = "wss") +
      geom_vline(xintercept = 8, linetype = 2)+
      labs(subtitle = "Elbow method")
    # Método silhouette para kmeans
    b <- fviz_nbclust(data, kmeans, method = "silhouette") +
      labs(subtitle = "Silhouette method")
    # Método gap statistic para kmeans
    c <- fviz_nbclust(data, MyKmeansFUN,  method = "gap_stat", nboot = 500,verbose = FALSE)+
      labs(subtitle = "Gap statistic method")
  }
  if (i==2){
    # Método elbow para kmodes
    a <- fviz_nbclust(data, kmodes, method = "wss") +
      geom_vline(xintercept = 9, linetype = 2)+
      labs(subtitle = "Elbow method")
    # Método silhouette para kmodes
    b <- fviz_nbclust(data, kmodes, method = "silhouette") +
      labs(subtitle = "Silhouette method")
    # Método gap statistic para kmodes
    c <- fviz_nbclust(data, kmodes,  method = "gap_stat", nboot = 500,verbose = FALSE)+
      labs(subtitle = "Gap statistic method")
  }
  return(list(a,b,c))
}

## Funci?n para crear gr?fico circular de la distribuci?n de los individuos en
# clusters.
dist_graph <- function(modAssig){
  freq <- tabulate(modAssig)
  for (i in 1:length(freq)){
    modAssig[modAssig==i] <- paste(i,paste0("(",round(freq[i]/sum(freq)*100,2),"%",")"), sep = " ")
  }
  pie <- ggplot(as.data.frame(modAssig), aes(x = "", fill = factor(modAssig))) + 
    geom_bar(width = 1) + coord_polar(theta = "y") +
    labs(fill="Cluster", 
         x=NULL, 
         y=NULL, 
         title="Proporciones") + theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  pie
}

## C-means

## Tabla con los valores de los distintos índices para seleccionar
 # el mejor modelo
 # Hace uso de los items utilizados para clusterizar
model_selection_table <- function(data){
  fuzzy_index <- c(1.1,1.2,1.4,1.6,1.8,2) ## Vector con ?ndices de fuzzificaci?n
  fuzzy_cluster <- c(2,3,4,5,6) ## Vector con n?mero de clusters
  ## Vectores para almacenar los valores de los ?ndices
  xb <- c()
  fs <- c()
  pc <- c()
  pe <- c()
  trownames <- c()
  for (i in 1:length(fuzzy_cluster)) {
    for (j in 1:length(fuzzy_index)) {
      model <- cmeans(data,fuzzy_cluster[i],FALSE,iter.max = 100,dist = "euclidean",method = "cmeans",fuzzy_index[j])
      xb <- c(xb,fclustIndex(model,data,"xie.beni"))
      fs <- c(fs,fclustIndex(model,data,"fukuyama.sugeno"))
      pc <- c(pc,fclustIndex(model,data,"partition.coefficient"))
      pe <- c(pe,fclustIndex(model,data,"partition.entropy"))
      ## Creo vector con el nombre de las filas para que tengan el formato
      # "N?mero de clusters"-"?ndice de fuzzificaci?n"
      trownames <- c(trownames,paste0(fuzzy_cluster[i],"-",fuzzy_index[j]))
    }
  }
  ## Creo el dataframe con los valores de los ?ndices
  Ktable <- as.data.frame(cbind(xb,fs,pc,pe))
  tcolnames <- c('XB','FS','PC','PE')
  ## Cambio nombres de columnas y de filas para poder identificar los valores
  # de la tabla
  colnames(Ktable) <- tcolnames
  rownames(Ktable) <- trownames
  ztable(Ktable)
}

## Data analysis
## LCA
## Función para separar datos en una dataframe diferente para cada cluster
cl_data_list <- function(data){
  res <- c()
  for (i in 1:max(data$Cluster3)){
    res[[i]] <- data[data$Cluster3==i,]
  }
  return(res)
}

## k-means, k-modes, c-means
 # Chi-cuadrado de Pearson para todas las variables utilizadas para clusterizar
chisq_items <- function(dataf){
  tbl_Sexo <- table(dataf$Cluster,dataf$Sexo)
  p_Sexo <- chisq.test(tbl_Sexo)$p.value
  tbl_ECivil <- table(dataf$Cluster,dataf$ECivil)
  p_ECivil <- chisq.test(tbl_ECivil)$p.value
  tbl_NFam <- table(dataf$Cluster,dataf$NFamiliar)
  p_NFam <- chisq.test(tbl_NFam)$p.value
  tbl_PEmp <- table(dataf$Cluster,dataf$PriEmp)
  p_PEmp <- chisq.test(tbl_PEmp)$p.value
  tbl_Edad <- table(dataf$Cluster,dataf$Edad)
  p_Edad <- chisq.test(tbl_Edad)$p.value
  tbl_Exp <- table(dataf$Cluster,dataf$Exp)
  p_Exp <- chisq.test(tbl_Exp)$p.value
  tbl_NEmp <- table(dataf$Cluster,dataf$NEmple)
  p_NEmp <- chisq.test(tbl_NEmp)$p.value
  tbl_NSoc <- table(dataf$Cluster,dataf$NSocios)
  p_NSoc <- chisq.test(tbl_NSoc)$p.value
  tbl_NHij <- table(dataf$Cluster,dataf$NHijos)
  p_NHij <- chisq.test(tbl_NHij)$p.value
  tbl_NEdu <- table(dataf$Cluster,dataf$NivelEd)
  p_NEdu <- chisq.test(tbl_NEdu)$p.value
  res <- list("Sexo" = list(tbl_Sexo,p_Sexo),"ECivil" = list(tbl_ECivil,p_ECivil),
              "NFam" = list(tbl_NFam,p_NFam),"PEmp" = list(tbl_PEmp,p_PEmp),
              "Edad" = list(tbl_Edad,p_Edad),"Exp" = list(tbl_Exp,p_Exp),
              "NEmp" = list(tbl_NEmp,p_NEmp),"NSoc" = list(tbl_NSoc,p_NSoc),
              "NHij" = list(tbl_NHij,p_NHij),"NEdu" = list(tbl_NEdu,p_NEdu))
  return(res)
}

 # Calcula los valores latentes de los factores y los añade al dataframe
valores_latentes <- function(dataf){
  # CFA
  fit <- cfa(modelocfa, data=dataf, estimator="MLM")
  summary(fit, fit.measures=TRUE,  standardized = TRUE, rsquare= T)
  
  # Predicción valores latentes
  valoreslatentes <-lavPredict(fit)
  valoreslatentes <- as.data.frame(valoreslatentes)
  dataf <- cbind(dataf,valoreslatentes)
  dataf$Cluster <- as.factor(dataf$Cluster)
  return(dataf)
}

## Concordancia observada para el cálculo de Kappa de Cohen
obs_agreement <- function(crosstable){
  return(sum(diag(crosstable))/sum(crosstable))
}

## Concordancia esperada para el cálculo de Kappa de Cohen
exp_agreement <- function(crosstable){
  values1 <- colSums(crosstable)
  values2 <- colSums(t(crosstable))
  res <- 0
  for (i in 1:nrow(crosstable)){
    res <- res + values1[i]/sum(crosstable)*values2[i]/sum(crosstable)
  }
  return(res)
}