# Función que reproduce clustering mediante Análisis de Clases Latentes (LCA)
# Hace uso de los datos que incluyen los items para clusterizar.
My_lca <- function(my_data){
  library(xlsx)
  library(mirt)
  
  # Se extrae dataframe con los items para clusterizar y se calculan los modelos a comparar.
  cluster_items <- my_data[137:146]
  model1 <- mdirt(cluster_items, 2, itemtype = "lca", verbose = FALSE)
  model2 <- mdirt(cluster_items, 3, itemtype = "lca", verbose = FALSE)
  model3 <- mdirt(cluster_items, 4, itemtype = "lca", verbose = FALSE)
  model4 <- mdirt(cluster_items, 5, itemtype = "lca", verbose = FALSE)
  model5 <- mdirt(cluster_items, 6, itemtype = "lca", verbose = FALSE)
  # Se realiza tabla comparativa a partir de la lista de modelos.
  lcas <- list(model1,model2,model3,model4,model5)
  selection_table_lca(lcas,500)
  
  # Nombres de clústeres e items para funciones gráficas.
  classnames <- c('1','2','3','4','5')
  itemnames <- c('Sexo','ECivil','NFamiliar','PriEmp','Edad','Exp','NEmple','NSocios',
                 'NHijos','NivelEd')
  # Se generan y guardan las gráficas incluidas en la memoria.
  mult.param <- multinomial_graph(model2,3,classnames,itemnames)
  ggsave("LCA mirt/responses.png", mult.param, width = 7.5, height = 5, units = "in")
  class.prop <- proportion_graph(model2,3,classnames)
  ggsave("LCA mirt/proportions.png", class.prop, width = 7.5, height = 5, units = "in")
  
  # Se retorna un dataframe con los datos clusterizados, el error de clasificación y unas
  # dummys para realizar regresiones lineales.
  mod.groups <- cbind("Cluster" = modalassign(fscores(model2)),"error" = classError(model2)[[1]],
                      dummyVar(modalassign(fscores(model2)),"Cluster"))
  reg.data <- cbind(my_data,mod.groups)
  write.xlsx(reg.data,"DatosClusterizados.xlsx",sheetName = "LCA", append = TRUE)
  return(reg.data)
}

# Función que reproduce clustering mediante C-means.
# Hace uso de los datos que incluyen los items para clusterizar.
My_cmeans <- function(my_data){
  library(e1071)
  library(xlsx) 
  
  # Se eliminan obsevaciones para las que faltan respuestas. El algoritmo no funciona cuando
  # existen NA.
  my_data <- drop_na(my_data,137:146)
  cluster_items <- my_data[137:146]
  
  # Se codifican mediante one-hot los items nominales y se combina con los ordinales.
  datao <- one_hot(cluster_items[1:4])
  datao <- cbind(datao,cluster_items[5:10])
 
  # Se obtienen índices para seleccionar el modelo y se generan los gráficos de estos índices.
  indexes <- selection_table_cmeans(datao)
  
  index_graph(indexes$K,indexes$m,index_matrix(indexes$K,indexes$m,indexes$xb),"Xie-Beni")
  index_graph(indexes$K,indexes$m,index_matrix(indexes$K,indexes$m,indexes$fs),"Fukuyama-Sugeno")
  index_graph(indexes$K,indexes$m,index_matrix(indexes$K,indexes$m,indexes$pc),"Partition coefficient")
  index_graph(indexes$K,indexes$m,index_matrix(indexes$K,indexes$m,indexes$pe),"Partition entropy")
  
  # Se clusterizan las observaciones.
  model <- cmeans(datao,3,FALSE,iter.max = 100,dist = "euclidean",method = "cmeans",1.1)
  
  # Previamente, se separan las observaciones de cada cluster en distintos dataframes.
  dataux <- as.data.frame(cbind(my_data,"Cluster" = model$cluster))
  datac1 <- dataux[dataux$Cluster==1,]
  datac2 <- dataux[dataux$Cluster==2,]
  datac3 <- dataux[dataux$Cluster==3,]
  
  # Se generan los gráficos para el modelo final y se guardan.
  pie <- dist_graph(model$cluster)
  ggsave("Cmeans/proportions.png", pie, width = 7.5, height = 5, units = "in")
  plt1 <- response_graph(datac1[137:146],colnames(my_data)[137:146])
  ggsave("Cmeans/responsesc1.png", plt1, width = 7.5, height = 5, units = "in")
  plt2 <- response_graph(datac2[137:146],colnames(my_data)[137:146])
  ggsave("Cmeans/responsesc2.png", plt2, width = 7.5, height = 5, units = "in")
  plt3 <- response_graph(datac3[137:146],colnames(my_data)[137:146])
  ggsave("Cmeans/responsesc3.png", plt3, width = 7.5, height = 5, units = "in")
  
  # Se guardan los datos clusterizados y se retornan para análisis posteriores.
  write.xlsx(dataux,"DatosClusterizados.xlsx",sheetName = "Cmeans", append = TRUE)
  return(dataux)
}

# Función que reproduce clustering mediante K-means.
# Hace uso de los datos que incluyen los items para clusterizar.
My_kmeans <- function(my_data){
  library(xlsx)
  
  # Se eliminan obsevaciones para las que faltan respuestas. El algoritmo no funciona cuando
  # existen NA.
  my_data <- drop_na(my_data,137:146)
  cluster_items <- my_data[137:146]
  
  # Se transforamn solo las variables categ?ricas mediante one-hot, las ordinales
  # se mantienen igual.
  datao <- one_hot(cluster_items[1:4])
  datao <- cbind(datao,cluster_items[5:10])
  # Se generan gráficos para la selección del modelo y son guardados (Elbow, Gap statistic y
  # silhouette).
  mod_sel_graphs <- selection_graphs(datao,1)
  ggsave("Kmeans/elbow-kmeans.png", mod_sel_graphs[[1]], width = 7.5, height = 5, units = "in")
  ggsave("Kmeans/sil-kmeans.png", mod_sel_graphs[[2]], width = 7.5, height = 5, units = "in")
  ggsave("Kmeans/gap-kmeans.png", mod_sel_graphs[[3]], width = 7.5, height = 5, units = "in")
  
  # Se clusterizan las observaciones.
  model <- kmeans(datao,3)
  
  # Separo las observaciones de cada cluster.
  dataux <- as.data.frame(cbind(my_data,"Cluster" = model$cluster))
  datac1 <- dataux[dataux$Cluster==1,]
  datac2 <- dataux[dataux$Cluster==2,]
  datac3 <- dataux[dataux$Cluster==3,]
  
  # Se generan los graficos para el modelo final de tres clústeres y se guardan.
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
  
  # Guardar y devolver datos para análisis posteriores.
  write.xlsx(dataux,"DatosClusterizados.xlsx",sheetName = "Kmeans", append = TRUE)
  return(dataux)
}

# Función que reproduce clustering mediante K-means.
# Hace uso de los datos que incluyen los items para clusterizar.
My_kmodes <- function(my_data){
  library(xlsx)
  library(klaR)
  
  # Se eliminan obsevaciones para las que faltan respuestas. El algoritmo no funciona cuando
  # existen NA.
  my_data <- drop_na(my_data,137:146)
  cluster_items <- my_data[137:146]
  
  # Se generan gráficos para la selección del modelo y son guardados (Elbow, Gap statistic y
  # silhouette).
  mod_sel_graphs <- selection_graphs(cluster_items,2)
  ggsave("Kmodes/elbow-kmodes.png", mod_sel_graphs[[1]], width = 7.5, height = 5, units = "in")
  ggsave("Kmodes/sil-kmodes.png", mod_sel_graphs[[2]], width = 7.5, height = 5, units = "in")
  ggsave("Kmodes/gap-kmodes.png", mod_sel_graphs[[3]], width = 7.5, height = 5, units = "in")
  
  # Se clusterizan las datos.
  model <- kmodes(cluster_items,3,iter.max = 10,weighted = FALSE)
  
  # Separo las observaciones de cada cluster.
  dataux <- as.data.frame(cbind(my_data,"Cluster" = model$cluster))
  datac1 <- dataux[dataux$Cluster==1,]
  datac2 <- dataux[dataux$Cluster==2,]
  datac3 <- dataux[dataux$Cluster==3,]
  
  # Se generan los gráficos y se guardan.
  pie <- dist_graph(model$cluster)
  pie
  ggsave("Kmodes/proportions.png", pie, width = 7.5, height = 5, units = "in")
  plt1 <- response_graph(datac1[137:146],colnames(my_data)[137:146])
  plt1
  ggsave("kmodes/responsesc1.png", plt1, width = 7.5, height = 5, units = "in")
  plt2 <- response_graph(datac2[137:146],colnames(my_data)[137:146])
  plt2
  ggsave("kmodes/responsesc2.png", plt2, width = 7.5, height = 5, units = "in")
  plt3 <- response_graph(datac3[137:146],colnames(my_data)[137:146])
  plt3
  ggsave("kmodes/responsesc3.png", plt3, width = 7.5, height = 5, units = "in")
  
  # Guardar datos para análisis posteriores.
  write.xlsx(dataux,"DatosClusterizados.xlsx",sheetName = "Kmodes", append = TRUE)
  return(dataux)
}

# Función para la entropia.
# mod es el modelo de clases latentes para el cual se calcula la entropía.
entropy <- function(mod){
  en<--sum(fscores(mod)*log(fscores(mod)),na.rm = TRUE) 
  e<-1-en/(nrow(fscores(mod))*log(ncol(fscores(mod)))) 
  return(e)
}

# Función para categorizar las variables no categoricas y simplificar las ya categóricas.
# Hace uso data que son datos inicialmente leídos.
data_preparation <- function(data){
  # Los -1 que indican la falta del dato, son cambiados por NA.
  data[data<0] <- NA
  datafi <- data # Se guardan los datos iniciales para combinarlos a las nuevas variables generadas.
  data <- data %>% select(v001,v002,v003,v004,v005,v007,v009,v010,v012,v013)
  # Se calcula la edad de cada encuestado en base al año 2019.
  edad <- 2019-data$v002
  # Se categoriza la edad.
  edcat <- edad
  edcat[edcat<=40] <- 1
  edcat[edcat<=60 & edcat>40] <- 2
  edcat[edcat>60 & edcat<=120] <- 3
  # Se categoriza la experiencia.
  expcat <- data$v009
  expcat[expcat<6] <- 1
  expcat[expcat>=6 & expcat<16] <- 2
  expcat[expcat>=16] <- 3
  # Se categoriza el número de empleados.
  nempcat <- data$v012
  nempcat[nempcat<=5] <- 1
  nempcat[nempcat>5 & nempcat<=20] <- 2
  nempcat[nempcat>20] <- 3
  # Se categoriza el ´numero de socios.
  nsocat <- data$v013
  nsocat[nsocat>=1] <- 2
  nsocat[nsocat==0] <- 1
  # Se categoriza el número de hijos.
  nhijcat <- data$v004
  nhijcat[nhijcat==0 & nhijcat<=2] <- -1
  nhijcat[nhijcat>2] <- 2
  nhijcat[nhijcat==-1] <- 1
  # Se simplifica el nivel educativo (se reducen categorías).
  educat <- data$v005
  educat[educat<=3] <- 1
  educat[educat>3 & educat<=5] <- 2
  educat[educat>5] <- 3
  # Elimino valores extraños.
  data$v003[data$v003==0] <- NA
  data$v003[data$v003>20] <- NA
  ## Sumo uno a las variables con 0 porque la librería para LCA no los acepta.
  data$v010 <- data$v010+1
  data$v007 <- data$v007+1
  # Se genera el dataframe con la combinación de las nuevas variables categóricas 
  # y los datos iniciales y se asignan los nombres.
  data2 <- cbind(datafi,data$v001,data$v003,data$v010,
                 data$v007,edcat,expcat,nempcat,nsocat,nhijcat,
                 educat)
  colnames(data2) <- c(colnames(datafi),'Sexo','ECivil','NFamiliar','PriEmp','Edad',
                       'Exp','NEmple','NSocios','NHijos','NivelEd')
  return(data2)
}

# Crear dummys para regresiones y como auxiliar para la codificación one-hot
# dataCol columna de datos que se corresponde con la variable para la que se requieren dummys. 
# nombre es el nombre de la variable, se utiliza para generar un nombre del estilo “nombre_número”
dummyVar <- function(dataCol,nombre){
  aux <- c() # Dummy
  res <- c() # Resultado
  cnames <- c() # Nombres de columnas
  # Se genera una dummy para cada valor de la variable
  for (i in 1:max(dataCol)){
    for (e in 1:length(dataCol)){
      if (dataCol[e]==i){
        aux[e] <- 1
      }else{
        aux[e] <- 0
      }
    }
    # Se va generando el dataframe final uniendo las dummys por columnas
    res <- cbind(res,aux)
    # Se genera la lista de nombres de columnas para asignarlos
    cnames <- c(cnames,paste0(nombre,"_",i))
  }
  colnames(res) <- cnames
  return (res)
}

# Función para el cálculo de la probabilidad de error de clasificación
# de cada observación, así como el total de todo del modelo.
# Hace uso de mod que es el modelo en cuestión.
classError <- function(mod){
  errors <- c()
  # Probabilidades de asignación de cada observación.
  probs <- fscores(mod)
  for (i in 1:nrow(probs)){
    # Para cada observación, la probabilidad de error es 
    # 1 menos la probabilidad de asignación mayor, es decir,
    # cuando asignación modal y probabilística no coinciden.
    errors[i] <- (1 - max(probs[i,]))
  }
  # Se calcula la proporción total de errores del modelo
  totError <- sum(errors)/length(errors)
  # Se crea una lista con ambos valores de interés para devolver como resultado.
  res <- list(errors,totError)
  return (res)
}

# Función para el cálculo el BLRT para cada par de modelos posible, es decir,
# para todos los modelos con K y K+1 clases. 
# Se realiza el bootstrap con nboot muestras diferentes, en caso de no
# especificarse el bootstrap se realizará con 500 muestras.
bootlrt <- function(lcas,nboot=NULL){
  library(doParallel)
  pValues <- c(0)
  # Creo distintas sesiones de R para la ejecución en paralelo.
  cl <- makeCluster(detectCores()-1)
  # Ejecuto las siguientes instrucciones en cada una de las sesiones.
  clusterEvalQ(cl,source("functions.R"))
  clusterEvalQ(cl,library(poLCA))
  clusterEvalQ(cl,library(mirt))
  # Ejecuto cada iteración del bucle en paralelo y combino los resultados en
  # un vector.
  registerDoParallel(cl)
  aux <- foreach (i=1:(length(lcas)-1), .combine = "c") %dopar% {
    # Ejecuto la función que calcula el estadístico para cada par de modelos.
    bootlrt_auxiliar(lcas[[i]],lcas[[i+1]],nboot)
  }
  # Cierro las distintas sesiones de R
  stopCluster(cl)
  # Combino los resultados obtenidos con el vector resultado.
  pValues <- c(pValues,aux)
  return(pValues)
}

# Función para el cálculo del Bootstrap Likelihood Ratio Test (BLRT) entre dos modelos estimados.
# nboot es el número de muestras de bootstrap. 500 por defecto.
# mod0 es el primer modelo
# mod1 es el segundo modelo (con más clases latentes)
bootlrt_auxiliar <- function(mod0,mod1, nboot=500){
  library(poLCA)
  lrt0 <- anova(mod0,mod1,verbose=FALSE)$X2[2] # Calcula LRT
  cont <- 0 # Contador para el cálculo del p valor (Proporción de LRT mayores o iguales al original)
  tot <- 0 # Contador de modelos que convergen, es decir, que se maximiza la función de log-likelihood.
  for (i in 1:nboot){
    # Creo una muestra del tamaño de la orginal a partir de las probabilidades condicionales y
    # las probabilidades a priori.
    datsim <- poLCA.simdata(N=NROW(fscores(mod0)),probs = summary(mod0)[1:10],P = summary(mod0)[[11]][,ncol(summary(mod0)[[11]])])[[1]]
    # Estimo dos nuevos modelos con las misma condiciones que los originales con la nueva muestra.
    modellr <- mdirt(datsim,NCOL(fscores(mod0)),itemtype = "lca",verbose = FALSE, technical = list(warn=FALSE, message=FALSE))
    modelmr <- mdirt(datsim,NCOL(fscores(mod1)),itemtype = "lca",verbose = FALSE, technical = list(warn=FALSE, message=FALSE))
    # Se comprueba si el modelo converge
    if (!extract.mirt(modellr, "converged")) next
    if (!extract.mirt(modelmr, "converged")) next
    tot <- tot + 1
    lrt <- anova(modellr,modelmr,verbose=FALSE)$X2[2]
    # Incremento el contador si se cumple la condición mencionada anteriormente
    if(lrt>=lrt0) cont <- cont+1
  }
  # Calculo el p valor como la proporción del estadístico que cumple la condición.
  pvalue <- (1+cont)/(1+nboot)
  return(pvalue)
}

# Crear tablas para la selección del modelo.
# lcas es una lista con los modelos estimados.
# nboot es el número de muestras de bootstrap para BLRT
selection_table_lca <- function(lcas,nboot=NULL){
  library(gridExtra)
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
  # Se guarda la tabla como imágen
  png(filename = "LCA mirt/SelectionTableLCA.png", width = 65*ncol(Ktable), height = 25*nrow(Ktable))
  grid.table(Ktable)
  dev.off()
}

# Función para el tratamiento de los resultados para realizar las gráficas en LCA.
# probs se corresponde con los parámetros multinomiales en LCA.
# ncl es el número de clústeres.
# clnames es la lista con el nombre de las clases.
# predcl es una columna de datos con la asignación modal de las observaciones a los clústeres.
# classDist se corresponde con la proporción de las clases o probabilidad a priori
graphic_data_preparation <- function(probs=NULL, ncl=NULL, clnames=NULL, predcl=NULL, classDist=NULL){
  # Se comprueba si es el gráfico sobre las proporciones
  if (!is.null(predcl)){
    predclass <- predcl
    for (i in 1:ncl){
      predclass[predclass==i] <- paste0(clnames[i]," ","(",round(classDist[i]*100,2),"%",")")
    }
    return(predclass)
  }
  # Se convierten las matrices de probabilidades en un dataframe
  # para poder darle el formato que necesito para graficarlos
  probs.df <- as.data.frame(probs)
  # Se obtienen los nombres de las columnas
  colnames2 <- names(probs.df)
  # Comienza tratamiento de datos para realizar gráficos
  probs2 <-  probs.df%>%pivot_longer(cols = colnames2, values_to = "value")
  # Se crea un vector con los nombres de los grupos para añadir al dataframe.
  # Se necesita más adelante ya que se pierde esta información en el proceso.
  classvector <- c()
  for(i in 1:ncl){
    classvector <- c(classvector, rep(clnames[i], nrow(probs2)/ncl))
  }
  probs2$class <- classvector
  # Trato la columna 'name'
  # Tiene el siguiente formato 'ECivil.category_1' y separo la columna en dos.
  # el indicador en la columna item y en response la respuesta.
  probs2 <- separate(probs2, col = 'name', into = c('item','response'),sep = '\\.')
  probs2$response <- gsub("category_","",probs2$response)
  return(probs2)
}

# Función que realiza el grafico sobre los patrones de respuesta de cada clase, también
# llamadas probabilidades condicionales P(X=t|Y) para lca.
# Hace uso del modelo estimado, el n?mero de clusters y los nombres de 
# los clusters.
multinomial_graph <- function(mod,ncl,clnames,itemnames){
  library(ggplot2)
  probs <- graphic_data_preparation(summary(mod)[1:10],ncl,clnames)
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

# Función que realiza el gráfico circular sobre el porcentaje de individuos que hay en 
# cada clase.
# mod se trata del modelo estimado
# nclu es el número de clústers
# classnames es el nombre de las clases
proportion_graph <- function(mod, nclu, classnames){
  library(ggplot2)
  # Tratamiento previo de los datos
  aux2 <- graphic_data_preparation(ncl = nclu, predcl = modalassign(fscores(mod)), clnames = classnames, classDist = summary(mod)$Class.Probability$prob)
  # Gráfico circular
  pie <- ggplot(as.data.frame(modalassign(fscores(mod))), aes(x = "", fill = factor(aux2))) + 
    geom_bar(width = 1) + coord_polar(theta = "y") +
    labs(fill="Clase", 
         x=NULL, 
         y=NULL, 
         title="Proporciones de clase") + theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  pie
}

# Función que realiza la asignación modal de las observaciones.
# Hace uso de probs que son las probabilidades de pertenecer a cada cluster.
modalassign <- function(probs){
  res <- c()
  # Para cada fila de probabilidades, que se corresponde a cada observación
  for (i in 1:nrow(probs)){
    # Se concatena al resultado la posición de la mayor probabilidad, es decir,
    # si la mayor probabilidad es la primera columna, se asigna al primer clúster.
    res <- c(res,which.max(probs[i,]))
  }
  return(as.vector(res))
}

# Función que tranforma un conjunto de variables a one-hot
# data es un dataframe para el cual cada columna se codificará mediante one-hot
one_hot <- function (data){
  res <- c()
  for (i in 1:ncol(data)){
    res <- cbind(res,dummyVar(data[,i],colnames(data)[i]))
  }
  return(res)
}

# Función para obtener la frecuencia de aparición de las respuestas en un cluster
# data es un dataframe compuesto por los items para clusterizar
# cnames es un vector con los nombres de estos items
response_frequency <- function(data,cnames){
  library(rowr)
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

# Función para obtener un gráfico con la frecuencia de aparición de las respuestas 
# en un cluster concreto.
# data es un dataframe con los items para clusterizar
# cnames es un vector con los nombres de estos items.
response_graph <- function(data,cnames){
  library(ggplot2)
  freq <- response_frequency(data,cnames)
  item <- c(rep(1,length(cnames)),rep(2,length(cnames)),rep(3,length(cnames)))
  graph_data <- freq%>%pivot_longer(cols = colnames(freq), values_to = "value")
  graph_data <- cbind(graph_data,item)
  plt <- ggplot(graph_data)
  # Añado los ejes x e y, además especifíco que se trata de un gráfico de columnas
  plt <-  plt + aes(x = graph_data$item, y = graph_data$value) + geom_col() +
    facet_grid(factor(graph_data$name, levels = cnames))+
    theme(text=element_text(size=8)) +
    # Añado la escala del eje Y (frecuencias) de 0 a 1.
    scale_y_continuous(breaks=c(0.5,1),labels=c(0.5,1)) +
    xlab('Respuesta')+ylab('Frecuencia')
  plt
}

# Función auxiliar para realizar los gráficos de selección de Kmeans
MyKmeansFUN <- function(x,k){
  kmeans(x, k, iter.max=50)
}

# Función que genera los gráficos de los distintos métodos de selección
# del número de clústeres
# data es dataframe con los items utilizados para clusterizar
# i es una variable que me permite pivotar entre los algoritmos kmeans (1) y kmodes (2)
# para generar los gráficos para el que corresponde. Kmeans por defecto
selection_graphs <- function(data,i=1){
  library(factoextra)
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

# Función para crear gráfico circular de la distribucion de los individuos en clusters.
# modAssig es un vector de datos que se corresponde con la asignación modal de las observaciones.
dist_graph <- function(modAssig){
  library(ggplot2)
  # Se obtienen frecuencia de asignación a cada cluster (proporciones).
  freq <- tabulate(modAssig)
  # Se genera una columa con el número de clúster y la frecuencia en porcentaje de ese clúster
  # para que la leyenda se genere con ese formato.
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

# Función que genera una matriz K x m para generar el gráfico del índice.
# K es el vector con los valores para número de clústeres.
# m es el vector con los valores para el parámetro de borrosidad.
# val es un vector con los valores del índice.
index_matrix <- function(K,m,val){
  res <- matrix(0,nrow = length(K),ncol = length(m))
  val_index <- 1
  for (i in 1:length(K)){
    for (j in 1:length(m)) {
      res[i,j] <- val[val_index]
      val_index <- val_index+1
    }
  }
  return(res)
}

# Función para realizar los gráficos de los indices utilizados para la selección en C-means
# K es un vector con los posibles valores para el número de clústeres.
# m es un vector con los posibles valores para el parámetro de borrosidad.
# i_matrix es una matríz K x m con los valores de un índice.
# index_name es el nombre del índice para el que se está generando el gráfico.
index_graph <- function(K,m,i_matrix,index_name){
  library(plotly)
  ex <- list(
    title = "m",
    range = c(1.1,2)
  )
  ey <- list(
    title = "N clusteres",
    tickvals=K,
    ticktext=K
  )
  ez <- list(
    title = "Indice"
  )
  sc <- list(xaxis=ex,yaxis=ey,zaxis=ez)
  p <- plot_ly(x = m, y = K, z = i_matrix, colors = "Accent") %>% add_surface() %>% layout(title = index_name, scene = sc)
  print(p)
}

# Función para generar tabla con los valores de los distintos índices para seleccionar el 
# mejor modelo (parámetros óptimos)
# Hace uso de los items utilizados para clusterizar
selection_table_cmeans <- function(data){
  library(gridExtra)
  fuzzy_index <- c(1.1,1.2,1.4,1.6,1.8,2) # Vector con índices de borrosidad
  fuzzy_cluster <- c(2,3,4,5,6) # Vector con número de clusters
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
      # Se crea vector con el nombre de las filas para que tengan el formato
      # "Número de clusters"-"Índice de borrosidad".
      trownames <- c(trownames,paste0(fuzzy_cluster[i],"-",fuzzy_index[j]))
    }
  }
  # Se genera el dataframe con los valores de los índices
  Ktable <- as.data.frame(cbind(xb,fs,pc,pe))
  tcolnames <- c('XB','FS','PC','PE')
  # Se cambian los nombres de columnas y de filas para poder identificar los valores
  # de la tabla.
  colnames(Ktable) <- tcolnames
  rownames(Ktable) <- trownames
  Ktable <- round(Ktable, 2)
  # Se guarda tabla como imagen.
  png(filename = "Cmeans/SelectionTableCmeans.png", width = 65*ncol(Ktable), height = 25*nrow(Ktable))
  grid.table(Ktable)
  dev.off()
  # Se retorna un dataframe con los valores de los índices para generar gráficos.
  res <- list("m" = fuzzy_index, "K" = as.factor(fuzzy_cluster), "xb" = xb, "fs" = fs,
              "pc" = pc, "pe" = pe)
  return(res)
}

# Función para separar datos en una dataframe diferente para cada cluster.
# data se corresponde con una dataframe con las observaciones clusterizadas.
cl_data_list <- function(data){
  res <- c()
  for (i in 1:max(data$Cluster)){
    res[[i]] <- data[data$Cluster==i,]
  }
  return(res)
}

# Chi-cuadrado de Pearson para todas las variables utilizadas para clusterizar
# dataf es una lista de dataframes que incluye los items utilizados para clusterizar
# para cada algoritmo
chi_squared_items <- function(dataf){
  res <- c()
  for (i in 1:length(dataf)){
    tbl_Sexo <- table(dataf[[i]]$Cluster,dataf[[i]]$Sexo)
    p_Sexo <- chisq.test(tbl_Sexo)$p.value
    tbl_ECivil <- table(dataf[[i]]$Cluster,dataf[[i]]$ECivil)
    p_ECivil <- chisq.test(tbl_ECivil)$p.value
    tbl_NFam <- table(dataf[[i]]$Cluster,dataf[[i]]$NFamiliar)
    p_NFam <- chisq.test(tbl_NFam)$p.value
    tbl_PEmp <- table(dataf[[i]]$Cluster,dataf[[i]]$PriEmp)
    p_PEmp <- chisq.test(tbl_PEmp)$p.value
    tbl_Edad <- table(dataf[[i]]$Cluster,dataf[[i]]$Edad)
    p_Edad <- chisq.test(tbl_Edad)$p.value
    tbl_Exp <- table(dataf[[i]]$Cluster,dataf[[i]]$Exp)
    p_Exp <- chisq.test(tbl_Exp)$p.value
    tbl_NEmp <- table(dataf[[i]]$Cluster,dataf[[i]]$NEmple)
    p_NEmp <- chisq.test(tbl_NEmp)$p.value
    tbl_NSoc <- table(dataf[[i]]$Cluster,dataf[[i]]$NSocios)
    p_NSoc <- chisq.test(tbl_NSoc)$p.value
    tbl_NHij <- table(dataf[[i]]$Cluster,dataf[[i]]$NHijos)
    p_NHij <- chisq.test(tbl_NHij)$p.value
    tbl_NEdu <- table(dataf[[i]]$Cluster,dataf[[i]]$NivelEd)
    p_NEdu <- chisq.test(tbl_NEdu)$p.value
    chi_items <- list("Sexo" = list(tbl_Sexo,p_Sexo),"ECivil" = list(tbl_ECivil,p_ECivil),
              "NFam" = list(tbl_NFam,p_NFam),"PEmp" = list(tbl_PEmp,p_PEmp),
              "Edad" = list(tbl_Edad,p_Edad),"Exp" = list(tbl_Exp,p_Exp),
              "NEmp" = list(tbl_NEmp,p_NEmp),"NSoc" = list(tbl_NSoc,p_NSoc),
              "NHij" = list(tbl_NHij,p_NHij),"NEdu" = list(tbl_NEdu,p_NEdu))
    res[[i]] <- chi_items
  }
  names(res) <- names(dataf)
  return(res)
}

# Calcula el test de Kruskal-Wallis para los constructos calculados por factores y composite
# y para cada algorítmo de clustering
# datos es una lista de dataframes con los datos para todos los algoritmos de clustering
kruskal_test <- function(datos){
  res <- c()
  for (i in 1:length(datos)){
    datos[[i]]$Cluster <- as.factor(datos[[i]]$Cluster)
    # Valores latentes por composite
    KWia <- kruskal.test(intenabandonar ~ Cluster, data = datos[[i]])
    KWco <- kruskal.test(compulsivo ~ Cluster, data = datos[[i]])
    KWex <- kruskal.test(exceso ~ Cluster, data = datos[[i]])
    KWnd <- kruskal.test(nodisfrute ~ Cluster, data = datos[[i]])
    KWcomposite <- cbind("intenabandono" = KWia,"compulsivo" = KWco,
                         "exceso" = KWex,"nodisfrute" = KWnd)
    # Valores latentes por factores
    KWia <- kruskal.test(intenabandonar_cfa ~ Cluster, data = datos[[i]])
    KWco <- kruskal.test(compulsivo_cfa ~ Cluster, data = datos[[i]])
    KWex <- kruskal.test(exceso_cfa ~ Cluster, data = datos[[i]])
    KWnd <- kruskal.test(nodisfrute_cfa ~ Cluster, data = datos[[i]])
    KWfactores <- cbind("intenabandono" = KWia,"compulsivo" = KWco,
                        "exceso" = KWex,"nodisfrute" = KWnd)
    KWres <- list("KWcfa" = KWfactores, "KWcomp" = KWcomposite)
    res[[i]] <- KWres
  }
  names(res) <- names(datos)
  return(res)
}

# Calcula los valores latentes de los constructos mediantes Confirmatory Factor Analysis (CFA)
# y los añade al dataframe.
# dataf es un dataframe con los datos inicialmente leidos
valores_latentes_cfa <- function(dataf){
  library(lavaan) # CFA para calculo de valores latentes
  modelocfa <-"
  compulsivo_cfa =~ v038 +v039 + v040 + v041 + v042 + v043 + v044 + v045 + v046 + v047 + v048
  exceso_cfa =~ v025 +v026 + v027 + v028 + v029 + v031 
  nodisfrute_cfa =~ v032 + v033 + v034 + v035 + v036  
  intenabandonar_cfa =~ v128 + v129 + v130 + v131 +v132 
  "
  # CFA
  fit <- cfa(modelocfa, data=dataf, estimator="MLM")
  summary(fit, fit.measures=TRUE,  standardized = TRUE, rsquare= T)
  
  # Predicción valores latentes
  valoreslatentes <-lavPredict(fit)
  valoreslatentes <- as.data.frame(valoreslatentes)
  return(valoreslatentes)
}

# Concordancia observada para el cálculo de Kappa de Cohen
# crosstable es la tabla cruzada entre dos algoritmos de clustering
obs_agreement <- function(crosstable){
  return(sum(diag(crosstable))/sum(crosstable))
}

# Concordancia esperada para el cálculo de Kappa de Cohen
# crosstable es la tabla cruzada entre dos algoritmos de clustering
exp_agreement <- function(crosstable){
  values1 <- colSums(crosstable)
  values2 <- colSums(t(crosstable))
  res <- 0
  for (i in 1:nrow(crosstable)){
    res <- res + values1[i]/sum(crosstable)*values2[i]/sum(crosstable)
  }
  return(res)
}