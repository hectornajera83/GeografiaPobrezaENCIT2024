setwd("D:/Rcurso")

 # install.packages("cowplot")
 # install.packages("ggthemes")

# Paquetes 
library(cowplot) # Unir graficos 
library(ggthemes) # Temas para personalizar graficos
library(tidyverse)
library(tidyr)
library(foreign)

# APLICACIONES DPLYR, PIPE, GGPLOT ####

enigh <- read.dbf("concentradohogar.dbf")
enigh <- as_tibble(enigh) 

# jefes del hogar: cuantos son hombres y cuantos son mujeres, por estrato socioeconomico
colnames(enigh)
enigh %>% select(factor)

# Estrato socioeconomico: 
# 1 muy bajo
# 2 bajo
# 3 medio bajo
# 4 alto



 enigh %>% 
  group_by(sexo_jefe,est_socio) %>% 
  summarise(Frecuencia=n(), 
            Frec_estimada = sum(factor)) %>%  #Con summarise va a resumir y n() me indicara cuantos elementos tengo
  mutate(sexo=ifelse(sexo_jefe==1,"Hombre","Mujer")) %>%  #ifelse, si se cumple la condicion, 
                                                      #imprime "Hombre", si no, imprime "Mujer"
  ggplot(aes(x=sexo,y=Frecuencia)) + #Recuerda que el primer elemento es el eje X y el segundo y 
  geom_col(aes(fill=sexo),col="black") + # NO OLVIDAR QUE todo lo que esta en AES esta en la base
  facet_wrap(~est_socio) + #Divide la visualizacion por estrato 
  theme_clean()  + # Aplicar un tema (explorar mas temas por su cuenta)
  labs(title = "Estrato socio economico por sexo", #titulo
       caption="Elaboracion propia con base en datos de la ENIGH 2022") +
  geom_text(aes(label=Frecuencia), vjust=-.5) # vjust es la justificacion vertical 
                                              # Negativo: Arriba 
                                              # Positivo: Abajo
 

 # coeficiente de variacion 
 # 0-15 alta precision 
 # 15:30 mediana precisión
 # mayor a 30 baja precisión
 

# AGREGANDO VARIABLE ESTADO 

enigh %>% 
  mutate(Estado=substring(ubica_geo,1,2)) %>%  ##vamos a usar substring para separar datos 
  select(ubica_geo, sexo_jefe, edad_jefe, Estado) %>% 
  head(10)

# ingreso corriente por sexo por entidad  
# stacked 
names(enigh)

enigh %>% 
  mutate(Estado=substring(ubica_geo,1,2)) %>% 
  group_by(Estado,sexo_jefe) %>% 
  summarise(ingreso_prom = mean(ing_cor), 
            ingreso_estimado = weighted.mean(ing_cor,w = factor)) %>%
  ggplot(aes(x=Estado,y=ingreso_estimado)) +
  geom_col(aes(fill=sexo_jefe),col="black", position = "dodge")+
  labs(title = "Promedio del ingreso corriente por sexo de los jefes de familia")

# Comparativo de ingreso corriente por sexo
# Dodged 
colnames(enigh)

enigh %>% 
  mutate(Estado=substring(ubica_geo,1,2)) %>% 
  group_by(Estado,sexo_jefe) %>% 
  summarise(Total_de_ingreso_por_sexo=mean(ing_cor)) %>% 
  ggplot(aes(Estado,Total_de_ingreso_por_sexo))+
  geom_col(position = "dodge",aes(fill=sexo_jefe),col="black")+
  labs(title = "Promedio del ingreso corriente por sexo del jefe de familia")


# Comparativo de ingreso corriente por sexo y estrato socioeconomico en profesionales
enigh  %>% 
  filter(educa_jefe %in% c(10,11)) %>% #Profesionales completos y Posgrado
  mutate(Estado=substring(ubica_geo,1,2)) %>%  
  group_by(Estado,sexo_jefe,est_socio) %>% 
  summarise(Total_de_ingreso_por_sexo=mean(ing_cor)) %>% 
  ggplot(aes(Estado,Total_de_ingreso_por_sexo))+
  geom_col(position = "dodge",aes(fill=sexo_jefe),col="black")+ #dodge
  labs(title = "Promedio del ingreso corriente por sexo y estrato socioeconomico")+
  theme_minimal() +
  facet_wrap(~est_socio, scales = "free") # scales="free" ajusta la escala para cada una 
                                          # de las divisiones 


## Cdmx 9, Nuevo Leon 19, Queretaro 22, Sonora 26, Yucatan 31

enigh %>% 
  dplyr::select(ubica_geo,sexo_jefe, est_socio,educa_jefe,9,11:13,24,factor,ing_cor,sueldos,ingtrab) %>% 
  mutate(Estado=substring(ubica_geo,1,2)) %>% 
  filter(Estado %in% c("09", "19", "22", "26", "31")) %>% 
  group_by(Estado,sexo_jefe) %>% 
  summarise(Total_de_ingreso_por_sexo=mean(ing_cor)) %>% 
  ggplot(aes(Estado,Total_de_ingreso_por_sexo)) +
  geom_col(position = "dodge",aes(fill=sexo_jefe),col="black")+
  labs(title = "Promedio del ingreso corriente por sexo",
       x= "Entidad Federativa",
       y= "Promedio del ingreso corriente",
       caption = "Elaboracion propia con datos de la ENIGH 2018") +
  coord_flip() +# coord_flip() # Gira los ejes 
  #theme_light() #Pueden aplicar diferentes temas de ggplot2 
  ggthemes::theme_excel_new() #Temas del paquete ggthemes 


# EJERCICIO GRAFICAS ####

# Base: ENIGH 2018
# Con la funcion mutate crea la variable "Estado" de la variable "ubica_geo"

# Crea la variable "Sexo" de la variable sexo_jefe con ayuda de (ifelse) #Hombre Mujer

# Crea la variable: Estrato_descrip con la informacion que viene en el descriptor de datos 
# usa ifelse para asignarlas. est_socio
# 1 = "Bajo"
# 2 = "Medio bajo"
# 3 = "Medio alto"
# 4 = "Alto" 

# con la funcion filter() selecciona las personas que han estudiado un posgrado. educa_jefe=11

# Agrupa la informacion por Estado y resume por Estado el promedio de ingreso por trabajo(ingtrab)

# Realiza la visualizacion con escala libre facet_wrap()
enigh  %>% 
  mutate(Estado=substring(ubica_geo,1,2), 
         Sexo = ifelse(sexo_jefe ==1,"Hombre","Mujer"),
         Estrato_descrip=ifelse(est_socio==1,"Bajo",
                                ifelse(est_socio==2,"Medio bajo",
                                       ifelse(est_socio==3,"Medio alto","Alto")))) %>% 
  filter(educa_jefe==11) %>% 
  group_by(Estado) %>% 
  mutate(promIngresoTrabajo=mean(ingtrab)) %>% 
  ggplot(aes(Estado,promIngresoTrabajo))+
  geom_col(position = "dodge",aes(fill=Sexo),col="black")+ #dodge
  labs(title = "Promedio del ingreso por trabajo y estrato socioeconomico de personas con posgrado")+
  theme_minimal()+
  facet_wrap(~Estrato_descrip, scales = "free")


# case_when 

enigh %>% 
  select(ubica_geo,edad_jefe) %>% 
  mutate(Test  = case_when(edad_jefe==74 ~ substring(ubica_geo,1,2), 
                           T ~ "No disponible"))

# case_when(
#   condicion1 ~ valor1,
#   condicion2 ~ valor2,
#   condicion3 ~ valor3,
#   TRUE ~ ValorParalodemas
# )


Grafica_casew <- enigh %>% 
  mutate(Estra_descrip = case_when(est_socio == 1 ~ "Bajo", 
                                   est_socio == 2 ~ "Medio bajo", 
                                   est_socio == 3 ~ "Medio alto",
                                   est_socio == 4 ~ "Alto"), 
         Estado=substring(folioviv,1,2),                        
         Sexo = ifelse(sexo_jefe==1, "Hombre", "Mujer")) %>% 
  filter(educa_jefe==11) %>% 
  group_by(Estado,Estra_descrip,Sexo) %>% 
  summarise(media_de_ingreso=mean(ingtrab)) %>% 
  ggplot(aes(Estado,media_de_ingreso))+
  geom_col(aes(fill=Sexo),col="black",position = "dodge")+
  facet_wrap(~Estra_descrip, scales = "free")


#UNIR DOS GRAFICOS EN UNO PARA PRESENTAR INFORMACION 

Nacional <- enigh %>% 
  mutate(Estado=substring(folioviv,1,2)) %>%
  filter(educa_jefe==11) %>% 
  group_by(Estado) %>%
  summarise(media_de_ingreso=mean(ingtrab)) %>% 
  
  ggplot(aes(reorder(Estado,-media_de_ingreso),media_de_ingreso)) + #Aplicar un orden a las columnas
  geom_col(fill="seagreen4",col="black") + 
  theme_minimal()

# Tenemos dos graficos, "Estrato" y "Nacional"
# Con el paquete cowplot unimos dos graficos 
cowplot::plot_grid()
plot_grid(Nacional,Grafica_casew, ncol = 1) 


# PIVOT WIDER Y PIVOT LONGER ####

# pivot_wider() nos permite ampliar los datos, aumentando el numero 
# de columnas y disminuyendo el numero de filas.

# id_cols : Un conjunto de columnas que identifica de forma unica cada observacion.
# y que no pivotan. Por default son todas las que no se especifican en names_from y
# values_from

# names_from : Columna de donde se obtienen los nombres 

# values_from: Columna(s) de donde se obtienen los datos 


enigh %>% 
  mutate(Estado=substring(folioviv,1,2)) %>% 
  group_by(Estado,sexo_jefe) %>% 
  summarise(media_ingreso=mean(ingtrab))


wider <- enigh %>% 
  mutate(Estado=substring(folioviv,1,2)) %>% 
  group_by(Estado,sexo_jefe) %>% 
  summarise(media_ingreso=mean(ingtrab)) %>% 
  mutate(Sexo = ifelse(sexo_jefe==1, "Hombre", "Mujer")) %>% 
  select(-sexo_jefe) %>% 
  pivot_wider(names_from = Sexo, 
              values_from = media_ingreso) 


# pivot_longer  "alarga" los datos aumentando el numero de filas 
# y disminuyendo el numero de columnas.

# cols : Columnas que se convertiran en una 

# names_to : "Nombre de la nueva columna para las categorias" 

# values_to : Nombre de la Columna donde van los valores 


wider #recordemos como se ve "wider"

wider %>%
  pivot_longer(cols = c(Hombre, Mujer), 
               names_to = "Sexo",
               values_to = "Media del ingreso") 


enigh %>% 
  count(sexo_jefe, educa_jefe) %>% 
  pivot_wider(names_from = educa_jefe, values_from = n)


enigh %>% 
  count(sexo_jefe, educa_jefe) %>% 
  pivot_wider(names_from = sexo_jefe, values_from = n) %>% 
  pivot_longer(cols = c(`1`,`2`), names_to = "Sexo", values_to = "n")

# EJERCICIO (base: Instrumentos de deuda) ####
deuda <- read.csv("instrumentos de deuda.csv")
colnames(deuda)
head(deuda)

#Cambiar los nombres para trabajar de forma mas facil 

colnames(deuda) <- c("Periodo", "Total", "Cetes", "Bondes",
                     "Udibonos", "Bonos_tasa_fija", "Otros_valores")

head(deuda) #Vean la estructura de la base de datos 

# Como graficar la emision de deuda por instrumento? 

# Utiliza pivot_longer para poder graficar correctamente los datos 

# Extra: Si puedes, resalta en la grafica los Cetes y Bondes de los demas instrumentos
deuda %>% 
  ggplot() +
  geom_line(aes(Periodo,Cetes, col="cetes")) +
  geom_line(aes(Periodo,Bondes,col="bondes")) 







