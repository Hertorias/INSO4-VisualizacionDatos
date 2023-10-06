#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(

  #Titulo de la pagina
  titlePanel("Pilotos con su vuelta rápida en cada circuito"),
  
  #Menú para seleccionar al primer piloto (no utiliza shinyWidgets)
  fluidRow(
    column(7, 
           wellPanel(selectInput(inputId = "driver",
              label = "Selecciona el primer piloto:",
              choices = sort(c("Charles Leclerc", "Carlos Sainz", "Lewis Hamilton", "George Russell", "Kevin Magnussen", 
                               "Valtteri Bottas", "Esteban Ocon", "Yuki Tsunoda", "Fernando Alonso", "Guanyu Zhou", 
                               "Mick Schumacher", "Lance Stroll", "Alexander Albon", "Daniel Ricciardo", "Lando Norris", 
                               "Nicholas Latifi", "Sebastian Vettel", "Pierre Gasly", "Max Verstappen", "Sergio Perez")),
              selected = "Fernando Alonso")
              ))),

  #Opciones de descarga y seleccion de otro piloto y/o de vuelta rápida
  column(5,
         fluidRow(downloadButton("downloadData", "Descargar los datos del piloto escogido")),
         fluidRow(checkboxInput("selectsecondcountry",label = "¿Seleccionar un piloto adicional?")),
         fluidRow(checkboxInput("selectfastestlap",label = "¿Ver las vueltas más rápidas?"))
  ),
  
  #Menú para seleccionar al segundo piloto
fluidRow(
  column(7,
         conditionalPanel("input.selectsecondcountry == true",
                          wellPanel(
                            selectInput(inputId = "secondDriver",
                                        label = "Selecciona el segundo piloto:",
                                        choices = sort(c("Charles Leclerc", "Carlos Sainz", "Lewis Hamilton", "George Russell", "Kevin Magnussen", 
                                                         "Valtteri Bottas", "Esteban Ocon", "Yuki Tsunoda", "Fernando Alonso", "Guanyu Zhou", 
                                                         "Mick Schumacher", "Lance Stroll", "Alexander Albon", "Daniel Ricciardo", "Lando Norris", 
                                                         "Nicholas Latifi", "Sebastian Vettel", "Pierre Gasly", "Max Verstappen", "Sergio Perez")),
                                        selected = "Carlos Sainz")
                          ))),
  
  # Panel principal con la tabla o el gráfico
  mainPanel(
    tabsetPanel(
      tabPanel("Tabla de datos",tableOutput("table")),
      tabPanel("Gráfico",plotOutput("formula1_plot"))
    ))

)))
