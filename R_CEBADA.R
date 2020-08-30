#####  #####   ####    ##      ####   ##
#####  ##      #  #    ###     ## ##  ###
##     ####    ###     # ##    ##  #  # ##
##     ####    ###     #####   ## ##  #####
#####  ##      #  #    #  ###  ####   #  ###
#####  #####   ###     #   ### ###    #   ###

#######R version 4.0.2 (2020-06-22)##########
#######Librerias####
#se utilizan estos paquetes, sino lo carga hay que instalarlos
library(BBmisc)
library(car)
library(dbplyr)
library(Hmisc)
library(RcmdrMisc)
library(corrplot)
library(agricolae)
library(tree)
library(boot)
library(dplyr)
install.packages(googledrive)
#Para evitar inconvenientes, actualizar los paquetes a su ultima versión
update.packages()#insertar el nombre del paquete entre parentesis

##########ETAPA INICIAL###########################
#partimos desde un analisis exploratorio de los datos. Primero se deben cargar,
#y operar con esa base datos.

###Analsis exploratorio#####

##comandos utiles para graficar
par(mfrow=c(1,1)) #numero de filas y columnas de salida en un plot

###cargo los datos 

Cebada <- read_delim("https://drive.google.com/uc?export=download&id=1dwymrFtBDtLfIg5QLK7ScXR3Vd2TL2fw",
                      "\t", escape_double = FALSE, trim_ws = TRUE)

Cebada <- select(Cebada, -X25, -X26, -X27, -X28, -X29) # Forma simple 2
na.omit(R_Cebada)
str(R_Cebada)
View(Cebada)

##Histogramas y tests de normalidad
with(Cebada, hist(ETr, scale="frequency", breaks=20, col="darkgray", xlab="mm", main="ETa3"))
shapiro.test(Cebada$Etr)
with(Cebada, hist(ETP, scale="frequency", breaks=20, col="darkgray", xlab="mm", main="ETa2"))
shapiro.test(Cebada$ETP)

###  Box plots
#ETa
Boxplot( ~ Etr, data=Cebada, id=list(method="y"), ylab="mm", main="ETa")
Boxplot( ~ ETP, data=Cebada, id=list(method="y"), ylab="mm", main="ETa")

#ahora estandarizamos los datos para ver las correlaciones entre las variables,
#Cuando r spearman es >0.7 o <-0.7

estand<-function(data, a, b)
{
  data$id<-as.numeric(rownames(data)) # asigno un identificador a la base original
  hola<-scale(data[,a:b], center=T, scale=T) # centro las variables
  hola2<-data.frame(hola) # convierto las columnas con las variables centradas en un data frame
  colnames(hola2)<-paste(names(hola2),"s", sep="") # le cambio los nombres a las columnas
  id<-as.numeric(rownames(hola2)) # genero el mismo identificador que a la base original
  hola3<-cbind(hola2, id) # uno el identificador con las columnas centradas
  basenueva<-merge(data, hola3, by="id") # junto ambos data frames a trav??s del id que gener??
  return(basenueva)
}
cebadastand<-estand(Cebada, 4, 24)
str(cebadastand)
View(cebadastand)

###### Correlaciones estandarizadas
coretas<-round(cor(cebadastand[,c(25:46)],method = ("spearman")),2)
corrplot(as.matrix(coretas), method="number")
rcorr(as.matrix(coreta), type=c("pearson","spearman"))

###### Normalizo los datos (x-min)/(max-min)

normal<-function(data, a, b)
{
  data$id<-as.numeric(rownames(data)) # asigno un identificador a la base original
  hola<-normalize((data[,a:b]), method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet") # normalizo las variables
  hola2<-data.frame(hola) # convierto las columnas con las variables centradas en un data frame
  colnames(hola2)<-paste(names(hola2),"n", sep="") # le cambio los nombres a las columnas
  id<-as.numeric(rownames(hola2)) # genero el mismo identificador que a la base original
  hola3<-cbind(hola2, id) # uno el identificador con las columnas centradas
  basenueva<-merge(data, hola3, by="id") # junto ambos data frames a trav??s del id que gener??
  return(basenueva)
}
cebadanorm<-normal(Cebada, 6, 59)
str(cebadanorm)
View(cebadanorm)

############## Correlaciones normalizadas
cornorm<-round(cor(cebadanorm[,c(63:100)],method = ("spearman")),2)
corrplot(as.matrix(cornorm), method="number")
pairs(Cebada)
pairs(cebadastand[,c(5,63:73)])
pairs(Cebada[,c(5,21:40)])
rcorr(as.matrix(coreta), type=c("pearson","spearman"))

###################
####Segundo Paso####
###################Û
#Quitamos las variables con tienen relación lineal y nos quedamos con las que consideramos
#importante

#Datos post analisis exploratorio. Aca se eliminaron las variables linealmente dependientes
Cebada2 <- select(Cebada, -Rain,	-T_10cm,	-T_20cm, -T_30cm_2, -T_40cm, -SWLower_Avg,	-LWLowerCo_Avg, -Ta_3m, -RH_3M, -WS_ms_3M_Avg) # Forma simple 2

str(Cebada2)
summary(Cebada2)
View(Cebada2)

# Correlaciones sin normalizar
cornorm<-round(cor(Cebada2[,c(3:14)],method = ("spearman")),2)
corrplot(as.matrix(cornorm), method="number")
pairs(Cebada2)
ggpairs(Cebada2[,c(3:14)])
rcorr(as.matrix(cornorm), type=c("pearson","spearman"))


#####con las variables definidas empiezo a modelar
##Hacercuadrados de las variables y ver su explicación

##tree
arbol<-tree(Etr~NDVI+T_5cm+SWUpper_Avg+LWUpperCo_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg+Trad_apo+Gcorregido, Cebada2)
arbol<-tree(Etr~SWUpper_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg, Cebada2)

summary(arbol)
plot(arbol)
text(arbol)

#####################################################
###Empiezo con los GLM (Modelos lineales generalizados)####
#####################################################

#####Procedimiento hacia ADELANTE (forward):  La selección comienza a partir del modelo más simple
#que se desee considerar (una  constante o tal vez la inclusión de una variable importante por
# fundamentos teóricos). El  modelo de inicio se especifica en el primer comando, y el scope (hasta 
#dónde seguir  incluyendo) se especifica a continuación.  

#Voy a probar con los GLM automaticos. En inicio voy a arrancar con Temp aire, radiacion entrante y humed rel., consideradas

######Empiezo el modelo con la temperatura del aire a 2m#####
#Datos basicos ("base"); 1=distribucion gaussian; 2=Distri. gamma
step(glm(Etr~Ta_2m, data= Cebada2, family=gaussian(link="identity")), 
     scope= Etr~SWUpper_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg, direction= "forward")
step(glm(Etr~Ta_2m, data= Cebada2, family=Gamma(inverse)), scope= Etr~SWUpper_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg, 
     direction= "forward")

#Todos los datos ("comp"); 1=distribucion gaussian; 2=Distri. gamma
step(glm(Etr~Ta_2m, data= Cebada2, family=gaussian(link="identity")), 
     scope= ETa1~NDVI+SWUpper_Avg+LWUpperCo_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg+Gcorregido, direction= "forward")
step(glm(Etr~Ta_2m, data= Cebada2, family=Gamma(inverse)), scope= Etr~NDVI+SWUpper_Avg+LWUpperCo_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg, 
     direction= "forward")

#GLM conseguidos en el paso anterior (mejor AIC)
GLMbase1 <- glm(formula = Etr ~ Ta_2m + SWUpper_Avg + WS_ms_2M_Avg, family = gaussian(link = "identity"), 
              data = Cebada2) {
  summary(GLMbase1)
}
          
GLMbase2<-glm(formula = Etr ~ Ta_2m + SWUpper_Avg + WS_ms_2M_Avg, family = Gamma(inverse), 
              data = Cebada2) {
summary(GLMbase2)
}
GLMcomp1<-glm(formula = Etr ~ Ta_2m + NDVI + LWUpperCo_Avg + Gcorregido + 
                WS_ms_2M_Avg, family = gaussian(link = "identity"), data = Cebada2) {
  summary(GLMcomp1)
}
GLMcomp2<- glm(formula = ETr ~ Ta_2m + NDVI + SWUpper_Avg, family = Gamma(inverse), 
               data = Cebada2) {
summary(GLMcomp2)
  }
  
#Verificación del modelo base (usar GLMbase1 o GLMbase2)
##base1####
vif(GLMbase1)
summary(GLMbase1)
qqnorm(resid(GLMbase1))
plot(GLMbase1)
  shapiro.test(resid(GLMbase1))
anova(GLMbase1)  
Anova(GLMbase1)
##base2####
vif(GLMbase2)
summary(GLMbase2)
qqnorm(resid(GLMbase2))
plot(GLMbase2)
shapiro.test(resid(GLMbase2))
anova(GLMbase2)  
Anova(GLMbase2)


#Verificación del modelo completo####
##completo1####
vif(GLMcomp1)
summary(GLMcomp1)
qqnorm(resid(GLMcomp1))
plot(GLMcomp1)
shapiro.test(resid(GLMcomp1))
anova(GLMcomp1)  
Anova(GLMcomp1)
##completo2####
vif(GLMcomp2)
summary(GLMcomp2)
qqnorm(resid(GLMcomp2))
plot(GLMcomp2)
shapiro.test(resid(GLMcomp2))
anova(GLMcomp2)  
Anova(GLMcomp2)

####verificación por medio del bootstraping (BS)####
bs <- function(formula, data, indices, familia) {
  d <- data[indices,]
  fit <- glm(formula, data=d, family=familia)
  return(coef(fit)) 
}
##### BS = GLMbase1 (gaussian)#####

results <- boot(data=Cebada2, statistic=bs, familia=gaussian, R=1000, formula = Etr ~ Ta_2m + SWUpper_Avg + WS_ms_2M_Avg)
summary(results)
plot(results, index=1) # oo
plot(results, index=2) # parámetro de Ta_2m
plot(results, index=3) # parámetro 
plot(results, index=4) # parámetro 

boot.ci(results, type="all", index=1) # oo
boot.ci(results, type="all", index=2) # 1er parámetro 
boot.ci(results, type="all", index=3) # 2do parámetro
boot.ci(results, type="all", index=4) # 3er parámetro

##### BS = GLMbase2 (Gamma)#####

results <- boot(data=Cebada2, statistic=bs, familia=Gamma, R=1000, formula = Etr ~ Ta_2m + SWUpper_Avg + WS_ms_2M_Avg)
summary(results)
plot(results, index=1) # oo
plot(results, index=2) # parámetro de Ta_2m
plot(results, index=3) # parámetro 
plot(results, index=4) # parámetro 

boot.ci(results, type="all", index=1) # oo
boot.ci(results, type="all", index=2) # 1er parámetro 
boot.ci(results, type="all", index=3) # 2do parámetro
boot.ci(results, type="all", index=4) # 3er parámetro

##### BS = GLMcomp1 (gaussian) ####
results <- boot(data=Cebada2, statistic=bs, familia=gaussian, R=1000, formula = Etr ~ Ta_2m + NDVI + LWUpperCo_Avg + Gcorregido + 
                  WS_ms_2M_Avg )
summary(results)
plot(results, index=1) # oo
plot(results, index=2) # parámetro
plot(results, index=3) # parámetro 
plot(results, index=4) # parámetro 
plot(results, index=5) # parámetro 
plot(results, index=6) # parámetro 

boot.ci(results, type="all", index=1) # oo
boot.ci(results, type="all", index=2) # 1er parámetro 
boot.ci(results, type="all", index=3) # 2do parámetro
boot.ci(results, type="all", index=4) # 3er parámetro
boot.ci(results, type="all", index=5) # 4to parámetro
boot.ci(results, type="all", index=6) # 5to parámetro


######BS = GLMcomp2 (gamma)#####

results <- boot(data=Cebada2, statistic=bs, familia=Gamma, R=1000, formula = ETa1 ~ Ta_2m + NDVI + SWUpper_Avg)
summary(results)
plot(results, index=1) # oo
plot(results, index=2) # parámetro de Ta_2m
plot(results, index=3) # parámetro de NDVI
plot(results, index=4) # parámetro de la variable 3

boot.ci(results, type="all", index=1) # oo
boot.ci(results, type="all", index=2) # 1er parámetro 
boot.ci(results, type="all", index=3) # 2do parámetro
boot.ci(results, type="all", index=4) # 3er parámetro
boot.ci(results, type="all", index=5) # 4to parámetro


######Empiezo el modelo con la radiacion entrante#####
step(glm(ETa3~SWUpper_Avg, data= Cebada2, family=gaussian(link="identity")), scope= ETa3~SWUpper_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg, direction= "forward")

step(glm(ETa3~SWUpper_Avg, data= Cebada2, family=gaussian(link="identity")), scope= ETa3~NDVI+T_5cm+SWUpper_Avg+LWUpperCo_Avg+DP+Ta_2m+RH_2M+WS_ms_2M_Avg+Trad.Ta+Gcorregido, direction= "forward")


######Empiezo el modelo con la humedad relativa#####
step(glm(ETa3~RH_2M, data= Cebada2, family=gaussian(link="identity")), scope= ETa3~SWUpper_Avg+Ta_2m+RH_2M+WS_ms_2M_Avg, direction= "forward")

step(glm(ETa3~RH_2M, data= Cebada2, family=gaussian(link="identity")), scope= ETa3~NDVI+T_5cm+SWUpper_Avg+LWUpperCo_Avg+DP+Ta_2m+RH_2M+WS_ms_2M_Avg+Trad.Ta+Gcorregido, direction= "forward")



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
###################ETAPA FINAL##################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

#Por medio de la verificación del modelo se observo que la temperatura del
#aire y la temperatura del suelo son variables dependientes (VIF>5). Con el bootstraping
#se obvservaron variables no significativas, por lo tanto se las retira 
#del modelo.

GLMbase1 <- glm(formula = Etr ~ Ta_2m + SWUpper_Avg, family = gaussian(link = "identity"), 
                data = Cebada2) 
summary(GLMbase1)

GLMbase2<-glm(formula = Etr ~ Ta_2m + SWUpper_Avg , family = Gamma(inverse), 
              data = Cebada2)
  summary(GLMbase2)

GLMcomp1<-glm(formula = Etr ~ Ta_2m + NDVI + LWUpperCo_Avg + Gcorregido 
                , family = gaussian(link = "identity"), data = Cebada2) 
  summary(GLMcomp1)

GLMcomp2<- glm(formula = ETa1 ~ Ta_2m + NDVI + SWUpper_Avg, family = Gamma(inverse), 
               data = Cebada2) 
  summary(GLMcomp2)

#Ahora agregamos los datos predichos a la base de datos
#ETreal= GLMbase1 (gaussian)
Cebada2$GLMbase1<-predict(GLMbase1, type="response")

#ETreal= GLMbase2 (gamma)
Cebada2$GLMbase2<-predict(GLMbase2, type="response")

#ETreal= GLMcomp1 (gaussian) 
Cebada2$GLMcomp1<-predict(GLMcomp1, type="response")

#ETreal= GLMcomp2 (gamma)
Cebada2$GLMcomp2<-predict(GLMcomp2, type="response")


######Creación del nuevo arcivo con los valores predictivos####
write.table(Cebada2, "Cebada2019GLM1.txt")

