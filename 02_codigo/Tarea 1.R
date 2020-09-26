# TAREA 1
# Arturo Aguilar

# Opciones ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") #mac

options(scipen = 999)

# Librerias a utilizar ----
install.packages("rmarkdown")

library(pacman)
library(leaflet) 
library(htmlwidgets)
library(webshot) 
library(DT)
library(kableExtra)
library(prettydoc)
library(rmarkdown)
p_load(tidyverse, 
       plotly,
       sf,
       viridis,
       readxl, 
       scales)


# Bases de datos ----

idh <- read_csv("01_Datos/Indice de Desarrollo Humano.csv")

mpios <- st_read("01_datos/mpios.geojson")

recreo <- read_csv("01_datos/denue_cvs.csv")

diamonds 


# 1. GRÁFICA y GRÁFICA INTERACTIVA EN GGPLOT ----

# Número de establecimientos recreativos (centros culturales, sociales, recreativos y deportivos) por estado. 

centros <- recreo %>% 
  group_by(entidad, tipo_asent, municipio) %>% 
  count()

centros <- recreo %>% 
  count(entidad) %>% 
  ggplot(aes(x = entidad,
             y = n,
             fill = entidad,
             alpha = 0.3,
             text= paste0("<b>Entidad:</b>", entidad, "<br>",
                          "<b>Establecimientos:</b>", n, "<br>"))) +
  geom_col() +
  coord_flip() +
  labs(title = "Servicios de esparcimiento culturales y deportivos, y otros servicios recreativos.",
       subtitle = "Año: 2019", 
       caption = "Fuente: DENUE") +
  theme(legend.position = "none")

centros

ggplotly(tooltip = "text")  %>% 
  config(displayModeBar = F)




# 2. MAPA ESTÁTICO EN GGPLOT Y VERSIÓN INTERACTIVA----

# MAPA con el valor del IDH para los municipios del estado de Chiapas y Oaxaca para el año 2015. 

# Generamos base de datos 
idh_chiapas <- idh %>% 
  filter(Year == 2015) %>% 
  filter(Entidad == "Chiapas")

mpios_chiapas <- mpios %>% 
  filter(NOM_ENT == "Chiapas")

# Combinamos las bases
mapa_chiapas <- merge(x = mpios_chiapas,
                     y = idh_chiapas,
                     by.x = "CVEGEO", 
                     by.y = "CODGEO", 
                     all.y = TRUE)

# Checamos que sea un objeto sf y que sea la geometría que queremos
class(mapa_chiapas)
plot(mapa_chiapas, max.plot = 1) 

# Hacemos el mapa en ggplot()

# La versión sencilla por tono de color en relación al Valor. 
mapa_chiapas %>% 
  ggplot(aes(fill = Valor)) + 
  geom_sf() 

# Versión 2 en la que metemos la paleta Viridis.
mapa_chiapas %>% 
  ggplot(aes(fill = Valor)) + 
  geom_sf() + 
  scale_fill_gradientn(colors = viridis(begin = 0, 
                                        end = 1, 
                                        n = 10))

# Versión 3 con información y limpiando la presentación-diseño del mapa e incluyendo información para interactividad así como labels, temas, etc.
mapa_chiapas %>% 
  ggplot(aes(fill = Valor,
             text= paste0("<b>Municipio: </b>", Municipio, "<br>",
                          "<b>Valor: </b>", Valor, "<br>"))) + 
  geom_sf(color = "gray80") + 
  scale_fill_gradientn(colors = viridis(begin = 0, 
                                        end = 1, 
                                        n = 10)) + 
  labs(title = "Índice de Desarrollo Humano <br>para el Estado de Chiapas",
       subtitle = "Año: 2015", 
       caption = "Fuente: Informe de Desarrollo Humano Municipal 2010-2015.") + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  theme(axis.text = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(),
        axis.ticks = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  colour = "gray10", 
                                  family = "Arial", 
                                  face = "bold", 
                                  size = 15), 
        plot.subtitle = element_text(hjust = 0.5, 
                                     colour = "gray50", 
                                     family = "Arial", 
                                     face = "bold", 
                                     size = 15), 
        plot.caption = element_text(colour = "gray50", 
                                    hjust = 1))

  ggplotly(tooltip = "text")  %>% 
    config(displayModeBar = F)

  

# 3. TABLA ---- 
  
  # PRESENTAMOS UNA TABLA CON LAS CARACTERÍSTICAS DE 10 DIAMANTES EXCLUSIVOS A LA VENTA PARA APOYAR AL CIDE. 
  
  dt <- diamonds[1:10, 1:7]
  
  kbl(dt)
  
  dt %>%
    kbl() %>%
    kable_paper("hover", 
                full_width = F)
  
  
  