#Manipulacion de la base de datos

library(dplyr)
library(foreign)
library(psych)
library(ggplot2)

#definir el directorio de trabajo 
setwd("C:/Users/Alumno/Documents/Taller")
#Cargar la base de datos. 
concentrado<-read.dbf("concentradohogar.dbf")

#Ver las variables contenidas en el  objeto 
names(concentrado)

#Generar una sub-base de la base principal. 
#seleccionar las variables de interes en la base  de datos

datos<-select(concentrado, folioviv, foliohog, ubica_geo, 
              tam_loc,est_socio, est_dis,   
              upm, factor, clase_hog, 
              sexo_jefe, edad_jefe, educa_jefe,
              tot_integ, hombres, mujeres,   
              mayores, menores, p12_64,   
              p65mas,ocupados, percep_ing,
              perc_ocupa,ing_cor, ingtrab )

#Crear nuevas variables a traves del verbo de "mutate"

datos <-mutate(datos, ictpc=ing_cor/tot_integ)

#Analisis descriptivo
#Medidas de tendencia central: media, mediana, moda. 
mean(datos$ing_cor)

#Realizar el analisis descriptivo. 
describe(datos)

#Determinar el tipo de variable
class(datos$folioviv)
class(datos$ing_cor)

#Usar el comando de filter para filtrar los datos a partir de
#una caracteristica en particular 

datos2000 <- filter(datos, ing_cor<2000)

#filtrar una nueva base que contiene toda la informacion 
#de las observaciones cuyo ingreso es menor a 2000

datosm <- filter(datos, sexo_jefe==2)

#Comando de pipe %>%

ing_promsex <- datos %>%
     group_by(sexo_jefe) %>%
     summarise(prom_sex=mean(ing_cor))

#Entidad federativa la vamos a obtener  a partir del folioviv. 

substr(datos$ubica_geo,1,2)

datos<-mutate(datos, ent=substr(ubica_geo,1,2))

#Analisis descriptivo de ingreso por entidad federativa. 
# generar ingresos promedios por entidad federativa. 

ingresoent<- datos %>%
   group_by(ent) %>%
   summarise(prom_ent=mean(ing_cor),
             min_ent=min(ing_cor),
             max_ent=max(ing_cor))

#Seleccionar zacatecas 

zac<- filter(datos, ent=="32")

#Diagrama de puntos

diagramapuntos <- zac %>%
  ggplot(aes(x=ing_cor, y=ingtrab))+
           geom_point(col="red")

#Diagrama de puntos con titulo

diagramapuntos <- zac %>%
  ggplot(aes(x=ing_cor, y=ingtrab))+
  geom_point(col="red")+
  labs(title = "Ingreso corriente total de zacatecas")

diagramapuntos

#Estratos socioeconomicos en zacatecas

g1 <- zac %>%
  ggplot(aes(x=est_socio))+
  geom_bar(col="blue")

#predominancia de jefes o  jefas de familia 

g2 <- zac %>%
  ggplot(aes(x=sexo_jefe))+
  geom_bar()


#Grafica de caja 

g3 <- zac %>%
  ggplot(aes(x=est_socio, y=ing_cor))+
  geom_boxplot()

g4 <- zac %>%
  ggplot(aes(x=sexo_jefe, y=ing_cor))+
  geom_boxplot()

zacmenos100 <- filter(zac, ing_cor<100000)

g5 <- zacmenos100 %>%
  ggplot(aes(x=sexo_jefe, y=ing_cor))+
  geom_boxplot()