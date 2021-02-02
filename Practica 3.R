# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Carga de paquetes/librerías necesarias.
function_install_packages <- function(x)
{
  for(i in x)
  {
    if(!require(i, character.only = TRUE))
    {
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
}

function_install_packages(c("dplyr", "ggplot2", "corrplot", "ggcorrplot", "e1071", "GGally", "tidyverse", 
                            "ggpubr", "base", "car", "MASS", "leaps", "hier.part", "gvlma", "lmtest",
                            "caTools", "pROC", "ROCR", "WRS2", "readxl", "stringi", "digest", "VIM", "mosaic"))



# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importamos dataset
dataset1 <- read.csv(sep = ";","C:/Users/34662/Escritorio/MÁSTER DATA SCIENCE/Material Máster Data Science/2 - Estadística/Carpeta Trabajos y borradores prácticas/PRACTICA 3/winequality-white.csv")
View(dataset1)
str(dataset1)
names(dataset1)

head(dataset1)
summary(dataset1)

# 1) Realizar una breve auditoría de los datos, determinando las cantidades de registros duplicados y
# vacíos, y elimandno esas duplicidades para generar la tabla protagonista del problema.

aggr(dataset1, numbers = TRUE)
summary(aggr(dataset1, numbers = TRUE)) # Mostramos si hay valores perdidos en alguna variable

# Deteccion de columnas con al menos un registro con valor nulo:
sapply(dataset1, function(x) all(!is.na(x)))

# Obtenemos que ninguna variable contiene un registro nulo.

# Funcion para conocer si una variable concreta esta totalmente documentada o si contiene nulos:
NA_COUNT<-function(x){
  for(i in 1:length(x)){
    if(is.na(x[i])) {
      print("Algun valor de la variable es nulo")
      break}
  }}

NA_COUNT(dataset1$fixed.acidity)
NA_COUNT(dataset1$volatile.acidity)
NA_COUNT(dataset1$citric.acid)
NA_COUNT(dataset1$residual.sugar)
NA_COUNT(dataset1$chlorides)
NA_COUNT(dataset1$free.sulfur.dioxide)
NA_COUNT(dataset1$total.sulfur.dioxide)
NA_COUNT(dataset1$density)
NA_COUNT(dataset1$pH)
NA_COUNT(dataset1$sulphates)
NA_COUNT(dataset1$alcohol)
NA_COUNT(dataset1$quality)

# Corroboramos que no disponemos de variables con valores nulos, ya que no nos aparece el print "Algun valor de la variable es nulo".

# Deteccion de duplicados basandonos en todas las columnas:
nrow(dataset1[duplicated(dataset1), ])     # 937 filas duplicadas
nrow(dataset1[!duplicated(dataset1), ])    # 3961 filas no duplicadas.

# Contruccion de la tabla final correctamente documentada.
dataset2 <-dataset1[!duplicated(dataset1)&(!is.na(dataset1$quality)), ]
aggr(dataset2, numbers = TRUE)
summary(dataset2$quality) # Corroboramos que no existen valores nulos (Na)

# 2) La variable a predecir será la calidad (quality) del vino, con lo que se pretende disponer de 
# este atributo en formato factor (inicialmente admite valores entre 3 y 9, indicando así el grado de calidad).

str(dataset2)

dataset2$quality <- factor(dataset2$quality, labels = c("Muy_Malo", "Malo", "Regular",
                           "Aceptable", "Bueno", "Muy_bueno", "Excelente"))
str(dataset2)
head(dataset2)

# 3) Una vez transformada esa variable a formato factor, codificarla de forma que indique alta calidad para 
# todos aquellos valores por encima de 6, y baja calidad para los casos con etiqueta menor o igual a 6. De este modo, 
# se habrá creado una nueva variable binaria (1/0) que represente la alta o baja calidad de cada registro (el formato deberá ser también factor).

dataset2$calidad_vino <- ifelse(dataset2$quality=="Muy_Malo", 0,
                                      ifelse(dataset2$quality=="Malo", 0,
                                             ifelse(dataset2$quality=="Regular", 0,
                                                    ifelse(dataset2$quality=="Aceptable", 0,
                                                           ifelse(dataset2$quality=="Bueno", 1,
                                                                  ifelse(dataset2$quality=="Muy_bueno", 1,
                                                                         ifelse(dataset2$quality=="Excelente", 1,
                                                                                "NULL")))))))
head(dataset2)
dataset2$valoracion_calidad <- NULL
tail(dataset2)

# Ahora ya estan clasificados por alta calidad (1) y baja calidad (0)
# Pasamos el formato de la variable generadada a factor.
str(dataset2)
dataset2$calidad_vino <- factor(dataset2$calidad_vino, labels = c("Baja", "Alta"))
str(dataset2)
head(dataset2)

# Correlaciones lineales.
cor(dataset1[ , c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)])
ggcorrplot(cor(dataset1[ , c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)], use="complete.obs"), hc.order=TRUE, type="lower", lab=TRUE)

# Boxplots avanzados
boxplot_top <- function(x,y)
{
  stats=boxplot.stats(x)$stats
  f=fivenum(x)
  stats2<-c(f[1],stats,f[5])
  stats3<-c(f[1],f[5])
  
  boxplot(x,main=y,col="steelblue2")
  abline(h=stats[1],lty=2,col="orange")
  abline(h=stats[5],lty=2,col="orange")
  text(rep(1.35,5),stats,labels=c('BIGOTE INFERIOR','PRIMER CUARTIL','MEDIANA','TERCER CUARTIL','BIGOTE SUPERIOR'))
  text(rep(.5,7),stats2,labels=round(stats2,digits=4),cex=0.6)
  text(rep(0.75,2),stats3,labels=c('MÍNIMO','MÁXIMO'))
}
# Visualizamos los diagramas de caja para una de las variables.
boxplot_top(dataset1$quality, 'Calidad del vino')


# 4) Determinar los porcentajes de vinos con alta calidad y baja calidad en el conjunto de datos sin duplicados.

# Creamos tabla en la que aparece cuantos registros hay por intervalo. Frecuencia absoluta:
tabla <- table(dataset2$calidad_vino)
addmargins(tabla)

# Sacamos la frecuencia relativa:
tabla_frec <- prop.table(table(dataset2$calidad_vino))*100
addmargins(tabla_frec)

# Observamos un boxplot para cada tipo de calidad del vino (alta o baja) y su distribución.
ggplot(data = dataset2, aes(x = calidad_vino, y = quality, color = quality)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

# 5) Partir esa misma tabla sin duplicados en dos conjuntos de datos: uno de training (70% de los registros)
# para construir un modelo predictivo que permita clasificar vinos en función de su alta o baja calidad, y 
# otro de test (30% de los registros) para realizar las correspondientes pruebas de la eficiencia en la predicción.

# Generamos dataframes para training y set

smp_size <- floor(0.70 * nrow(dataset2))

set.seed(123)
train_ind <- sample(seq_len(nrow(dataset2)), size = smp_size)

train <- dataset2[train_ind, ]
test <- dataset2[-train_ind, ]

# 6) Desarrollar con el cojunto de training una regresión logística múltiple que relacione la variable binaria de 
# la calidad del vino con el resto de variables descriptivas de la tabla.

# Empleamos el comando glm (para modelos lineales generalizados), indicando que la respuesta es binomial 
# (calidad vino alta o baja) mediante el argumento family:
# No incorporamos la variable "quality", ya que nos interesa obtener una predicción sin conocer el valor de esa variable.

modelo<-glm(calidad_vino~., data=train, family=binomial)
modelo2 <- update(modelo,.~.-quality)
summary(modelo2)

# 7) Seguir un proceso step para seleccionar las variables más representativas en la predicción. Describir
# el modelo final resultante.

# Proceso step (seleccionando vía p-valor).
#A través de la función step() conocemos de antemano que el modelo más apropiado es el que tiene un valor AIC con valor mínimo
step(modelo2, direction = "backward")    

# **calidad_vino** ~ 0.5273 * fixed.acidity -2.624 * volatile.acidity + 2.384 * residual.sugar - 
#    1.769 * chlorides + 0.01884 * free.sulfur.dioxide -0.003883 * total.sulfur.dioxide -
#     587.2 * density + 4.18 * pH + 1.944 * sulphates + 0.2464 * alcohol + 561.2

modelo_def <- glm(calidad_vino~fixed.acidity + volatile.acidity + residual.sugar + chlorides +
                    free.sulfur.dioxide + total.sulfur.dioxide + density + pH + 
                    sulphates + alcohol, data=train, family = binomial)
summary(modelo_def)

modelo_def

# Generamos los intervalos de confianza:
confint(modelo_def)
confint.default(modelo_def)

# Calculamos la variable independiente que más afección tiene sobre nuestra variable dependiente, donde vemos que se trata de
# alcohol y density.
relat.imp.RMSPE=hier.part(train$calidad_vino, train[, 1:11], 
                          family="gaussian", gof="RMSPE", barplot=TRUE)
relat.imp.RMSPE

range(train$alcohol)

# Predicciones de calidad_vino mediante la recta de regresión.
(nueva_calidad_vino<-data.frame(alcohol=seq(8.0, 14,2)))
predict(modelo_def, nueva_calidad_vino)

# Evaluamos el modelo con Likelihood ratio:

dif_residuos <- modelo_def$null.deviance - modelo_def$deviance
paste("Diferencia de residuos:", round(dif_residuos, 4))

# Grados libertad
df <- modelo_def$df.null - modelo_def$df.residual
paste("Grados de libertad:", df)

# P-VALOR:
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)
paste("p-value:", round(p_value, 4))

# El modelo en conjunto sí es significativo y, acorde a los p-values mostrados en el summary(), también es
# significativa la contribución al modelo de los distintos predictores.

# 8) Generar las probabilidades asociadas a las predicciones que corresponden al conjunto de test. Crear una nueva variable que 
# asigne a cada registro con probabilidad mayor que 1/3 la etiqueta de buen vino, siendo mal vino todo aquel que muestre una 
# probabilidad por debajo o igual a 1/3.

# Generamos modelo regresión a partir dataframe "TEST"

modelo3<-glm(calidad_vino~., data=test, family=binomial)
modelo4 <- update(modelo,.~.-quality)
summary(modelo4)

step(modelo4, direction = "backward")  

modelo_def2 <- glm(calidad_vino~fixed.acidity + volatile.acidity + residual.sugar +
                    free.sulfur.dioxide + density + pH + 
                    sulphates, data=test, family = binomial)
summary(modelo_def2)

# Para generar predicción: si la probabilidad de buen vino es superior a 0.33 se le asigna al nivel 1 (alta calidad)
# si la probabilidad es menor, se le asigna un 0 (baja calidad)

predicciones <- ifelse(test = modelo_def2$fitted.values > 0.33, 
                       yes = 1, no = 0)

matriz_confusion <- table(modelo_def2$model$calidad_vino, predicciones,
                          dnn = c("Observaciones", "Predicciones"))
matriz_confusion

+((840+120)/(840+120+107+122))*100

# Generamos variable de predicción con la función predict.

test$prediccion <- predict(modelo_def2, test, type = "response")

head(test) # Ya aparece la variable de prediccion para cada registro.

# Generamos nueva variable que nos indique la calidad del vino a partir de la probabilidad.
test$prediccion_calidad <- ifelse(test$prediccion > 1/3, "Alta",
                                  ifelse(test$prediccion <= 1/3, "Baja",
                                         "NULL"))

head(test) # Ahora ya nos aparece una variable de prediccion de la calidad alta o baja.

test$pred_calidad_grafico <- ifelse(test$prediccion_calidad == "Baja", 0,
                                    ifelse(test$prediccion_calidad == "Alta", 1,
                                           "NULL"))
test$pred_calidad_grafico <- as.numeric(as.character(test$pred_calidad_grafico))

hist(100*test$pred_calidad_grafico, col="skyblue2",
     main=" resultados modelo glm() sobre datos Test",
     xlab="Probabilidad en % de calidad alta o baja",
     ylab="Frecuencia")

# 9) Mediante una tabla cruzada, mostrar todas las combinaciones de predicciones y valores reales de la calidad del vino 
# (hablar de los resultados obtenidos para conceptos tales como precisión o acierto del modelo, así como de las proporciones
# de buenos y malos vinos reales que son detectados por el modelo correctamente).

# Una vez obtenemos los registros del conjunto de test
tabla2 <- table(test$calidad_vino, test$prediccion_calidad)
addmargins(tabla2)

# Sacamos la frecuencia relativa:
tabla_frec2 <- prop.table(table(test$calidad_vino, test$prediccion_calidad))*100
addmargins(tabla_frec2)

# Matriz de confusión: comparación modelo train y test.

summary(modelo_def)
summary(modelo_def2)

prediccion_valor <- predict(modelo_def2, test, type = "response")
prediccion_cal <- ifelse(prediccion_valor>1/3, "Alta", "Baja")
data_matriz <- data.frame(observed = test$calidad_vino,
                          predicted = prediccion_cal)

head(data_matriz)

# Matriz de confusión
calidad_alta <- sum(data_matriz$observed == "Alta")
calidad_baja <- sum(data_matriz$observed == "Baja")
prediccion_calidad_alta <- sum(data_matriz$predicted == "Alta")
prediccion_calidad_baja <- sum(data_matriz$predicted == "Baja")
total <- nrow(data_matriz)
data.frame(calidad_alta, calidad_baja, prediccion_calidad_alta, prediccion_calidad_baja)

tp<-sum(data_matriz$observed=="Alta" & data_matriz$predicted=="Alta")
tn<-sum(data_matriz$observed=="Baja" & data_matriz$predicted=="Baja")
fp<-sum(data_matriz$observed=="Baja" & data_matriz$predicted=="Alta")
fn<-sum(data_matriz$observed=="Alta" & data_matriz$predicted=="Baja")
data.frame(tp,tn,fp,fn)


# Calculos de precisión modelo
accuracy <- (tp+tn)/total
error_rate <- (fp+fn)/total
sensitivity <- tp/calidad_alta
especificity <- tn/calidad_baja
precision <- tp/prediccion_calidad_alta
npv <- tn / prediccion_calidad_baja
data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)




