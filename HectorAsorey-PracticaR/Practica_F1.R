#Carga de librerías
library(tidyverse)
library(ggplot2)

#Directorio donde esté el CSV
setwd("C:/Users/Hector/Desktop/Visualizacion de Datos/HectorAsorey-PracticaR")

#Leemos el dataset
dataframe_formula1 <- read.csv2(file = "Formula1_2022.csv", sep = ",")

#Sustituyo Austria por Autria para que posteriormente en los gráficos cuando abrevie por las tres primeras
#letras del circuito, se distinga AUS de AUT (Australia frente a Austria)
dataframe_formula1$Track[dataframe_formula1$Track == "Austria"] <- "Autria"
dataframe_formula1$Track <- factor(dataframe_formula1$Track, levels = unique(dataframe_formula1$Track))
head(dataframe_formula1)

View(dataframe_formula1)

#Obtenemos un dataframe donde estan los puntos que ha conseguido la escudería con sus dos pilotos en
#cada carrera
dataframePuntosEscuderia <- aggregate(Points ~ Team + Track, data = dataframe_formula1, sum)

#Simplificamos el nombre de los equipos
dataframePuntosEscuderia$Team <- gsub(" Ferrari", "", dataframePuntosEscuderia$Team)
dataframePuntosEscuderia$Team <- gsub(" Mercedes", "", dataframePuntosEscuderia$Team)
dataframePuntosEscuderia$Team <- gsub(" Aramco Mercedes", "", dataframePuntosEscuderia$Team)
dataframePuntosEscuderia$Team <- gsub(" Aramco", "", dataframePuntosEscuderia$Team)
dataframePuntosEscuderia$Team <- gsub(" Renault", "", dataframePuntosEscuderia$Team)
dataframePuntosEscuderia$Team <- gsub(" RBPT", "", dataframePuntosEscuderia$Team)
dataframePuntosEscuderia$Team <- gsub(" Racing", "", dataframePuntosEscuderia$Team)

#Abreviamos el nombre de los circuitos
dataframePuntosEscuderia$TrackAbbr <- substr(dataframePuntosEscuderia$Track, 1, 3)

#Hacemos factor para que el gráfico no ordene alfabéticamente y respete el orden de los circuitos
dataframePuntosEscuderia$TrackAbbr <- factor(dataframePuntosEscuderia$TrackAbbr, levels = unique(dataframePuntosEscuderia$TrackAbbr))

#Alfa Romeo, Alpha Tauri, Alpine, Aston Martin, Ferrari, Haas, McLaren, Mercedes, Red Bull, Williams
coloresEscuderias <- c("#B12039", "#4E7C9B", "#2293D1", "#2D826D", "#ED1C24", "#8C8F91", "#F58020",
                       "#6CD3BF", "#1E5BC6", "#37BEDD")

#Definimos un estilo para los gráficos
my_theme <- theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

#Gráfico 1: Tendencia de puntos por escuderí
ggplot(dataframePuntosEscuderia, aes(x = TrackAbbr, y = Points, group = Team, color = Team)) + 
  geom_smooth(se = FALSE, size=1.2) +
  labs(x = "Circuito", y = "Puntos obtenidos",
       title = "Tendencia de los puntos obtenidos a lo largo del campeonato por escudería") + my_theme +
  scale_color_manual(values = coloresEscuderias)

#Obtenemos un dataframe de los pilotos con los puntos obtenidos y lo ordenamos en orden descendiente
dataframePilotos <- aggregate(Points ~ Driver, data = dataframe_formula1, sum)
dataframePilotos <- dataframePilotos[order(-dataframePilotos$Points),]

#Nos quedamos con los cinco pilotos con más puntos para que sea más sencillo de ver el gráfico
Best5Pilots <- dataframePilotos[0:5, ]$Driver

#Obtenemos un dataframe de todos los datos de los cinco mejores pilotos
dataframeBest5 <- filter(dataframe_formula1, Driver %in% Best5Pilots)
dataframeBest5$TrackAbbr <- substr(dataframeBest5$Track, 1, 3)

#Como hay valores numericos y string, hay que hacer una conversión previa
dataframeBest5$Position <- factor(dataframeBest5$Position,
                                  levels = rev(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                                                 "13", "14", "15", "16", "17", "18", "19", "20", "NC")))
dataframeBest5$TrackAbbr <- factor(dataframeBest5$TrackAbbr, levels = unique(dataframeBest5$TrackAbbr))

#Charles Leclerc, George Russell, Lewis Hamilton, Max Verstappen, Sergio Perez
coloresPilotos <- c("#ED1C24", "#6CD3BF", "#05fac9", "#0552f7", "#2202a6")

#Gráfico 2: Posiciones de los pilotos en cada circuito
ggplot(dataframeBest5, aes(x = TrackAbbr, y = Position, group = Driver, color = Driver)) + 
  geom_line(size = 1.2) + 
  labs(x = "Circuito", y = "Posición",
       title = "Posición en la que termina cada piloto de los 5 mejores por circuito") +
  my_theme +
  scale_color_manual(values = coloresPilotos)