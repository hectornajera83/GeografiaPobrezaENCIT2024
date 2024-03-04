setwd("C:/Rcurso2023")

# install.packages("RColorBrewer") 
# install.packages("gganimate")
# install.packages("gifski") 
# install.packages("gapminder")
# install.packages("plotly")

# Paquetes a cargar 
library(tidyverse)
library(readxl)
library(gapminder)
library(foreign)
library(plotly)
library(RColorBrewer) #Paletas de colores 


# Base de datos 
pib <- read_excel("PIB_Entidades.xlsx") 
pib <- as_tibble(pib)

#Graficamos la tasa de crecimiento del PIBE por Entidad 

crecimientos <- pib %>% 
  filter(Entidad %in% c("Ciudad de Mexico", "Nuevo Leon", "Quintana Roo")) %>% 
  select(-PIBE_PC) %>% 
  group_by(Entidad) %>% 
  mutate(Rezago = lag(PIBE),          # lag() y lead() util para comparar valores detras o 
         adelante = lead(PIBE),       # delante de los valores actuales 
         Tasa_crecimiento = (PIBE-Rezago)/Rezago * 100) %>% 
  ggplot(aes(Year, Tasa_crecimiento,
             linetype = Entidad, 
             shape=Entidad,
             col=Entidad)) +
  geom_point() +
  geom_line(size=1) +
  scale_y_continuous(limits = c(-9,12),          
                     breaks = seq(-9,12,3),
                     labels = function(x)paste(x,"%", sep = "")) +      
  scale_x_continuous(limits = c(1980,2016),       
                     breaks = seq(1980,2016,1))          

crecimientos


# paste() Unir o concatenar vectores, valores 
paste("Clave", 2) # Los va a unir en un solo string 
paste("Clave", 2, sep="") #Puedes elegir el separador 
V1 <- "B"
V2 <- c(1:3)
paste(V2,V1, sep = "/")


#Puedes guardar el grafico en un objeto y seguir agregando capas 
crecimientos +  #Nombre del objeto donde guardamos nuestro grafico  
  labs(title = "Tasa de crecimiento del PIB por Entidad",        
       subtitle = "1980-2016", x="", y= "Porcentaje", 
       caption = expression(paste("Elaboracion propia con datos de ", italic("ENIGH 2018")))) +
  theme(plot.title = element_text(size = 15,face= "bold",color="brown4",hjust= 0.5,family = "serif"), 
        plot.subtitle = element_text(size = 14,color ="brown4",hjust = 0.5,face = "bold", family = "serif"),
        plot.caption = element_text(hjust = 0), # Mover Caption hacia la izquierda
        axis.text.x = element_text(size = 11, 
                                   angle = 45), 
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position = "bottom")

?ggplot2::theme()
# stackoverflow 

# italic = cursiva 
# face = Fuente ("plain", "italic", "bold", "bold.italic")
# hjust = horizontal justification (0-1)
# family = Tipo de letra ("Sans", "Times New Roman")
# element_blank() = Asigna ningun espacio 

# R Color Brewer (Paletas de colores)
library(RColorBrewer)
brewer.pal.info #Informaci?n de las paletas de colores 
display.brewer.all() #Ver las paletas en plots 

#Mostrar paletas por divisi?n 
display.brewer.all(type = "seq") #sequential 
display.brewer.all(type = "div") #Divergent 
display.brewer.all(type = "qual") #qualitative

#Mostrar opciones para dalt?nicos 
display.brewer.all(colorblindFriendly = T)

#Ejemplo de uso 

#Base de datos Enigh
enigh <- read.dbf("concentradohogar.dbf")
enigh <- as_tibble(enigh) 

## Cdmx 9, Nuevo Le?n 19, Queretaro 22, sonora 26, Yucatan 31
enigh %>% 
  mutate(Estado=substring(folioviv,1,2)) %>%
  filter(Estado %in% c("09", "19", "22", "26", "31")) %>% 
  filter(educa_jefe==11) %>%  #posgrado 
  group_by(Estado) %>%
  summarise(media_de_ingreso=mean(ingtrab)) %>% 
  ggplot(aes(reorder(Estado,-media_de_ingreso),media_de_ingreso, fill=Estado)) + 
  geom_col(col="black") +
  scale_fill_brewer(palette = "Spectral")


# Gradiente de color para variables continuas 
enigh %>% 
  mutate(Estado=substring(folioviv,1,2)) %>%
  filter(educa_jefe==11) %>% 
  group_by(Estado) %>%
  summarise(media_de_ingreso=mean(ingtrab)) %>% 
  ggplot(aes(reorder(Estado,-media_de_ingreso),media_de_ingreso, fill=media_de_ingreso)) + 
  geom_col(col="black") +
  scale_fill_gradient("Titulo de leyenda",low = "antiquewhite", high = "red4")


# ggplotly  
# Base de datos Gapminder ####

# Variables : 
#   Esperanza de vida, 
#   PIB per capita 
#   Poblacion por pais.

gapminder <- gapminder
head(gapminder)

unique(gapminder$year) #Anios 
unique(gapminder$country) # Paises (142 valores unicos)
unique(gapminder$continent)

#Geom_point
gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, color=continent)) +
  geom_point(aes(size=pop))+ 
  scale_x_log10()+ #aplicamos log base 10 al eje x para mejorar la visualizacion de los datos
  theme_light()+
  labs(x="GDP per capita",
       y="Esperanza de vida",
       title = "GDP per capita X Esperanza de Vida") 


# geom_smooth
gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, color=continent)) +
  geom_smooth(method=lm, se=T)+  #se es el intervalo de confianza
  theme_light()+
  scale_x_log10()+
  labs(x="GDP per capita",
       y="Esperanza de vida",
       title = "GDP per capita X Esperanza de Vida ") +
  ggthemes::theme_clean()

#method= loess


## Submuestra para comparar Europa y Africa 

colnames(gapminder)

gapminder %>% 
  filter(continent %in% c("Europe", "Africa")) %>% 
  ggplot(aes(gdpPercap, lifeExp, color=continent)) +
  geom_point(aes(size=pop)) +
  theme_light() +                                    
  scale_x_log10() +
  labs(x="GDP per capita",
       y="Esperanza de vida",
       title = "GDP per capita X Esperanza de Vida")


EU_AFR2 <- gapminder %>% 
  filter(continent %in% c("Europe", "Africa")) %>% 
  ggplot(aes(gdpPercap, lifeExp, color=continent)) +
  geom_point(aes(size=pop, frame= year, ids=country)) +   #frame = variable de tiempo 
  theme_light() +                                         #ids = Identificador
  scale_x_log10() +
  labs(x="GDP per capita",
       y="Esperanza de vida",
       title = "GDP per capita X Esperanza de Vida")

ggplotly(EU_AFR2)#Funcion del paquete plotly 


#Editando un poco el cuadro de informacion 

colnames(gapminder)
EU_AFR2 <- gapminder %>% 
  filter(continent %in% c("Europe", "Africa")) %>% 
  ggplot(aes(gdpPercap, lifeExp, color=continent, 
             text = paste0("Pais: ", country, 
                           "\nPoblacion: ", pop, 
                           "\nGDP per capita: ", gdpPercap, 
                           "\nEsperanza de vida: ", lifeExp))) +
  geom_point(aes(size=pop, frame= year, ids=country)) +   #frame = variable de tiempo 
  theme_light() +                                         #ids = Identificador
  scale_x_log10() +
  labs(x="GDP per capita",
       y="Esperanza de vida",
       title = "GDP per capita X Esperanza de Vida")

ggplotly(EU_AFR2, tooltip = c("text")) 

# Despues de correr la funcion, se guarda como un archivo html desde 
# Export > Save as a webpage 

# Con ese grafico se pueden sacar imagenes estaticas en determinados anios



