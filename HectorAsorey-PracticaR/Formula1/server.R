#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Carga de librerías
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)

shinyServer(function(input, output) {
  
    #Lectura y procesamiento de los datos
    dataframe_formula1 <- read.csv2(file = "Formula1_2022.csv", sep = ",")
    dataframe_formula1$Track[dataframe_formula1$Track == "Austria"] <- "Autria"
    dataframe_formula1$Track <- factor(dataframe_formula1$Track, levels = unique(dataframe_formula1$Track))
    dataframe_formula1$TrackAbbr <- substr(dataframe_formula1$Track, 1, 3)
    dataframe_formula1$TrackAbbr <- factor(dataframe_formula1$TrackAbbr, levels = unique(dataframe_formula1$TrackAbbr))
    
    #Hago que la columna de tiempos pase de formato texto a formato numérico, para poder alterar el orden
    #del eje en el grafico
    dataframe_formula1 <- transform(dataframe_formula1,
                                    Time = as.numeric(sub("(\\d+):(\\d+)\\.(\\d+)", "\\1.\\2\\3", Fastest.Lap)))
  
    #Obtengo un dataframe que me permita obtener la vuelta más rápida independientemente del piloto
    #Para ello, primero un dataframe sin DNF
    dataframeSinDNF <- subset(dataframe_formula1, !(Time.Retired %in% c("DNF", "DNS")))
    
    #Dataframe con las vueltas más rápidas
    dataframeOfFastest <- dataframeSinDNF %>%
      group_by(Track) %>%
      slice_min(Fastest.Lap)
    
    #En el gráfico aparecerá en la leyenda "Vuelta más rápida" como nombre del piloto
    dataframeOfFastest$Driver <- "Vuelta más rápida"
    
    #Concatenamos ambos dataframes
    dataframe_formula1 <- rbind(dataframe_formula1, dataframeOfFastest)
    
    #Lo convertimos a datatable
    dataTableF1 <- data.table(dataframe_formula1)

    #Tabla de datos que se muestra con el primer piloto escogido (por defecto: Fernando Alonso)
    output$table <- renderTable({
      dataTableF1[Driver == input$driver]
    })
    
    #Definimos el tema del gráfico
    my_theme <- theme_minimal() + 
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
    
    #Gráfico principal
    output$formula1_plot <- renderPlot({

        grafico <-
        ggplot()+
        geom_line(size = 1.2, data = dataTableF1[Driver == input$driver], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) + 
        geom_point(size = 2, data = dataTableF1[Driver == input$driver], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) +
        scale_y_reverse() + 
        labs(x = "Circuito", y = "Tiempo de vuelta rápida",
             title = "Pilotos con su vuelta rápida en cada circuito") +
        my_theme
      
      #Condiciones que determinan si se muestra a otro piloto y/o se muestra la vuelta rápida
       if (input$selectsecondcountry == TRUE & input$selectfastestlap == TRUE)
         grafico+
          geom_line(size = 1.2, data = dataTableF1[Driver == input$secondDriver], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataTableF1[Driver == input$secondDriver], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) +
          geom_line(size = 1.2, data = dataTableF1[Driver == "Vuelta más rápida"], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataTableF1[Driver == "Vuelta más rápida"], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) 
       
        else if (input$selectsecondcountry == TRUE & input$selectfastestlap == FALSE)
          grafico +
          geom_line(size = 1.2, data = dataTableF1[Driver == input$secondDriver], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataTableF1[Driver == input$secondDriver], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver))
        
        else if (input$selectsecondcountry == FALSE & input$selectfastestlap == TRUE)
          grafico +
          geom_line(size = 1.2, data = dataTableF1[Driver == "Vuelta más rápida"], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataTableF1[Driver == "Vuelta más rápida"], aes(x = TrackAbbr, y = Time,  color=Driver, group = Driver)) 
        
       else grafico

    })
    
    #Creamos un fichero que se descargará con los datos del primer piloto elegido
    output$downloadData <- downloadHandler(
      filename=function() {gsub(" ", "", paste("Data-",input$driver,"-",Sys.Date(),".csv"))},
      content= function(file){write.csv(dataTableF1[Driver == input$driver],file)}
    )

})
