setwd("D:/Curso R 2023")

# Paquetes 
library(tidyverse)
library(readxl)

#base de datos 
pib <- read_excel("PIB_Entidades.xlsx")
Titanic <- read.csv("Titanic.csv")
Titanic <- as_tibble(Titanic)
deuda <- read.csv("instrumentos de deuda.csv")
Mat <- read.csv("student-mat.csv")
Mat <- as_tibble(Mat)

# GGPLOT2 #### 

# Una grafica en ggplot consiste en un numero de componentes clave que obedecen un rango jerarquico:

# Dataframe: contiene toda la informacion que sera desplegada

# Aesthetics: aes() variables a graficar, atributos relacionados a los datos 

# Geoms: Son objetos geometricos tales como puntos, lineas, figuras, etc.)

# Atributos/capas extra


#               ggplot(datos, aes(x,y)) + figura_geometrica + capas_extra             #



# Vamos a trabajar primero con los datos de CDMX
cdmx <- filter(pib, Entidad == "Ciudad de Mexico")

colnames(cdmx)

cdmx %>% 
  ggplot(aes(x=Year, y=PIBE)) # Mapeo: todo lo que vaya dentro de aes()
                              # se encuentra en la base de datos 

cdmx %>% 
  ggplot(aes(x=Year, y=PIBE)) +  #Las capas se agregan con "+"
  geom_point() # capa geom 


cdmx %>% 
  ggplot(aes(x=Year, y=PIBE)) + 
  geom_point(col= "#74C7FB",   # Color 
             size = 5)            # Tamano

?geom_point


cdmx %>% 
  ggplot(aes(x=Year, y=PIBE)) + 
  geom_point(col="firebrick3", # Color 
             size = 3,         # Tamano 
             shape= 11,  # Forma
             alpha= .3)        # Transparencia


cdmx %>% 
  ggplot(aes(x=Year, y=PIBE)) + 
  geom_point(col="firebrick3",
             size = 3,         
             shape= 2,  #Shape puede codificarse como  nombre o numero de figura
             alpha= .5) +
  labs(title = "PIBE Ciudad de Mexico (Millones de pesos)", #labs permite anadir titulo
       subtitle = "1980-2016",           # subtitulo, etiquetas en ejes, 
       x= "periodo",                     # caption 
       y= "Millones de pesos", 
       caption = "Elaborado con datos de INEGI")


# Se pueden anadir otras geometrias en el mismo grafico
cdmx %>% 
  ggplot(aes(x=Year, y=PIBE)) + 
  geom_point(col="firebrick3", size = 3) +
  geom_line() 


# PIBE y PIBE_pc 
cdmx %>% 
  ggplot(aes(x=Year, y=PIBE)) + 
  geom_line(col="firebrick3", size = .5) +
  geom_line(aes(x=Year, y=PIBE_PC), col="blue",size=.5)


# PIBE_PC por Estados  (Notar que ahora usamos la base completa pib)
pib %>% 
  ggplot(aes(x=Year, y=PIBE_PC)) +
  geom_line(aes(col=Entidad)) # El color lo definimos segun el estado 
  

#Mismo resultado 
pib %>% 
  ggplot(aes(x=Year, y=PIBE_PC, col=Entidad)) +
  geom_point() +
  geom_line()


# Titanic ####
View(Titanic)
head(Titanic)

# Grafico de barras (Por default muestra frecuencias)
table(Titanic$survived)

Titanic %>% 
  ggplot(aes(x=survived)) +
  geom_bar() # aqui solo cuenta observaciones 


Titanic %>% 
  ggplot(aes(survived)) +
  geom_bar(col="red") #Cuando trabajamos con geometrias que tienen relleno 
                      #como barras/columnas tenemos que utilizar "fill".
                      # col se refiere al contorno de la figura 

Titanic %>% 
  ggplot(aes(survived))+
  geom_bar(fill="turquoise4",alpha=0.6, col="blue") #Agregamos color y se "fija" a un 
                            # VALOR CUALQUIERA, NO RELACIONADO A LA BASE DE DATOS 
                            # FUERA de la funcion aes()


# Graficamos Sexo 
Titanic %>% 
  ggplot(aes(x=sex))+
  geom_bar(aes(fill=sex), alpha=0.7) # En este caso, el color se fija a una VARIABLE 
# DENTRO de la funcion aes() pues la informacion se va a extraer 
# de la base de datos 


#El siguiente grafico nos da la misma grafica. Desde un principio fijamos el relleno
# con la variable "sex" 
Titanic %>% 
  ggplot(aes(x=sex, fill=sex)) +
  geom_bar(alpha=0.4)


# Por que no sucede lo mismo con la variable "survived" ? 
Titanic %>% 
  ggplot(aes(survived))+
  geom_bar(aes(fill=survived),alpha=0.7)

str(Titanic) #Nos damos cuenta que survived esta como entero y no como factor

Titanic$survived <- as.character(Titanic$survived)

Titanic %>% 
  mutate(survived =  as.character(survived)) %>% 
  ggplot(aes(survived))+
  geom_bar(aes(fill=survived),alpha=0.7)


#Podemos asignar colores, relleno y otros atributos a distintas variables 
str(Titanic)
Titanic$pclass <- as.character(Titanic$pclass)

Titanic %>%
  ggplot(aes(x=sex)) +
  geom_bar(aes(fill=pclass),alpha=0.7) #Position


# Elegir manualmente el color (Una vez cambiado survived a factor)
Titanic %>% 
  ggplot(aes(survived))+
  geom_bar(aes(fill=pclass),alpha=0.7) +
  scale_fill_manual(values=c("palevioletred3","steelblue", "red")) 


Titanic %>% 
  ggplot(aes(survived))+
  geom_bar(aes(fill=survived),alpha=0.7)+ 
  scale_fill_manual(values=c("palevioletred3", 
                             "steelblue")) +
  labs(title = "Personas que sobrevivieron",
       y="frecuencia", 
       caption = "Datos de ...") # Vamos agregando capas a nuestro grafico
                       # Labs sirve para agregar titulos, subtitulos, caption, 
                       # nombre de ejes, etc. 

Titanic %>% 
  mutate(Survived2 = ifelse(survived==1, "Sobrevivio", "No sobrevivio")) %>% 
  ggplot(aes(Survived2)) +
  geom_bar(aes(fill=Survived2))


# Grafico de caja #### 

Titanic %>% 
  ggplot(aes(x=sex, y=age))+
  geom_boxplot(aes(col=sex)) 

# histogramas 
colSums(is.na(Titanic))

Titanic %>% 
  ggplot(aes(age)) +
  geom_histogram(fill= "goldenrod", bins = 30)

Titanic %>% 
  ggplot(aes(age)) +
  geom_density(fill= "goldenrod")


# DIFERENCIA ENTRE GEOM_BAR Y GEOM_COL (Student mat)

# Instruccion: Graficar el numero de hombres y mujeres que hay por escuela
colnames(Mat)
head(Mat)
Mat %>% distinct(school)

table(Mat$sex, Mat$school) #1 
Mat %>% count(sex,school) #2

Mat %>% 
  group_by(sex, school) %>% 
  summarise(Total = n())

# Geom_bar por defecto cuenta el numero de casos 
Mat %>% 
  ggplot(aes(sex)) +
  geom_bar(aes(fill=school), position = "dodge")

# Mientras que geom_col grafica LA INFORMACION YA CALCULADA O QUE SE ENCUENTRA EXPLICITAMENTE EN EL DF
# Es decir ...

Mat %>% 
  count(sex,school) %>%  #Primero calculamos y a partir de la tabla graficamos 
  ggplot(aes(x=sex, y=n)) +
  geom_col(aes(fill=school), position = "dodge") +
  geom_text(aes(label=n))

# Instruccion: Graficar el puntaje promedio por condicion de tener una relacion amorosa y por sexo


Mat %>% 
  group_by(sex, romantic) %>% 
  summarise(Puntaje_promedio = mean(G3)) %>% 
  ggplot(aes(sex, Puntaje_promedio)) +
  geom_col(aes(fill=romantic), position = "dodge")


# A pesar de que geom_bar cuenta por defecto, podemos cambiar eso
Mat %>% 
  group_by(sex, romantic) %>% 
  summarise(Puntaje_promedio = mean(G3)) %>% 
  ggplot(aes(sex, Puntaje_promedio)) +
  geom_bar(aes(fill=romantic), stat = "identity", position = "dodge")


# Ejercicio ####
# Compara la calificacion final de matematicas de acuerdo al tiempo de estudio semanal de los 
# estudiantes (studytime) mediante una gráfica e interpreta. 

Mat %>% select(G3, studytime)

Mat %>% 
  group_by(studytime) %>% 
  summarise(mean(G3))

Mat %>% 
  mutate(studytime = as.factor(studytime)) %>% 
  ggplot(aes(studytime, G3)) +
  geom_boxplot()

Mat %>% 
  mutate(studytime = as.factor(studytime)) %>% 
  ggplot(aes(G3)) +
  geom_density(aes(fill=studytime), alpha=.5)

# Emision de deuda  ####
colnames(deuda)

#Cambiar los nombres para trabajar de forma mas facil 

colnames(deuda) <- c("Periodo", "Total", "Cetes", "Bondes",
                     "Udibonos", "Bonos_tasa_fija", "Otros_valores")

head(deuda) #Vean la estructura de la base de datos 


# Emision total 
deuda %>% 
  ggplot(aes(x=Periodo, y=Total)) + 
  geom_point(col="firebrick3", size=2) +
  geom_line(col="black")+
  labs(title = "Emision de deuda total",  
       subtitle = "1990-2006",
       x= "Anios",
       y= "Emision de deuda")


# Como graficar la emision de deuda por instrumento? 

# Podemos hacer esto, sin embargo no sabemos que colores estan asociados a
# los instrumentos 

deuda %>% 
  ggplot(aes(Periodo)) +
  geom_line(aes(y=Cetes),col="deepskyblue3", size=1) +
  geom_line(aes(y=Bondes), col= "firebrick3", size=1)+
  geom_line(aes(y=Udibonos), col= "goldenrod", size=1)+
  geom_line(aes(y=Bonos_tasa_fija), col= "forestgreen", size=1)

# 

deuda %>% 
  ggplot(aes(Periodo)) +
  geom_line(aes(y=Cetes, col="Cetes"), size=1) +
  geom_line(aes(y=Bondes, col="Bondes"), size=1)+
  geom_line(aes(y=Udibonos,col="Udibonos"), size=1)+
  geom_line(aes(y=Bonos_tasa_fija, col="Bonos tasa fija"), size=1)





